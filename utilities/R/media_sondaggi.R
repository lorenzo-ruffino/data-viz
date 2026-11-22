# ============================================================================
# Media bayesiana dei sondaggi, sul modello di quella di doppiacifra
# (italia-polls-average/02. Average/polling_average.stan), scritta in R puro
# perche' gira senza Stan.
#
#   stato:        mu_s(t) = mu_s(t-1) + w,              w ~ t_2(0, tau_s)
#   osservazione: y_i = mu_s(t_i) + delta_{s,j(i)} + e_i
#                 e_i ~ N(0, (se_i^2 + sigma_s^2) / peso_i)
#   istituti:     delta_{s,j} ~ t_4(0, omega_s)
#
# - mu_s(t) e' il consenso della serie s nel giorno t: un percorso che di
#   solito cambia poco da un giorno all'altro (passeggiata casuale con passo
#   tau_s). I passi hanno le code pesanti (t di Student con gl_passi = 2 gradi
#   di liberta'): quasi sempre piccoli, ogni tanto un salto. Con i passi
#   normali la media e' lenta: in una simulazione sul calendario dei sondaggi
#   spagnoli del 2026, dopo un salto vero di 2 punti in campagna ne registrava
#   il 57 per cento a tre giorni e il 69 a dieci; con la t a 2 gradi il 78 e
#   l'84, e nei periodi senza salti l'errore resta lo stesso (0,18 punti
#   contro 0,17);
# - ogni sondaggio ne e' un'osservazione imprecisa: l'errore campionario se_i
#   (dal campione, fino a n_max interviste) piu' un rumore extra sigma_s
#   stimato dai dati;
# - delta e' lo scarto sistematico dell'istituto (house effect). Gli scarti
#   sono relativi al consenso: stanno intorno a zero, ogni istituto conta come
#   una prova sola per quanti sondaggi pubblichi, e la distribuzione ha le code
#   pesanti (t di Student con 4 gradi di liberta'), cosi' un istituto fuori
#   scala (il CIS in Spagna, sette punti sotto gli altri sul PP) viene
#   riconosciuto come tale e non trascina il consenso dalla sua parte. Quanto
#   il consenso degli istituti sia centrato non si puo' sapere dai sondaggi:
#   l'incertezza sul livello che ne deriva e' dentro la fascia;
# - peso_i riduce il peso di chi pubblica molto: un sondaggio vale
#   1 / sqrt(k), dove k sono i sondaggi dello stesso istituto nei 14 giorni
#   intorno. Un tracker quotidiano non domina la media: sui sondaggi spagnoli,
#   togliendo uno alla volta gli otto istituti piu' prolifici il valore finale
#   si sposta al massimo di 0,3 punti. Un peso piu' severo (1 / k, esponente
#   = 1) riduce ancora la dipendenza ma dimezza la reattivita' in campagna.
#
# Prior: tau ~ N+(0, prior_tau), sigma ~ N+(0, prior_sigma),
#        omega ~ N+(0, prior_omega), mu(1) ~ N(media dei primi sondaggi, 10^2).
#
# Stima: campionamento di Gibbs. Per ogni catena e iterazione:
#   1. tau con un passo di Metropolis sulla verosimiglianza marginale del
#      filtro di Kalman (mu integrato via), poi mu con il campionamento
#      all'indietro (FFBS): tau e mu si aggiornano insieme, la catena non si
#      incaglia. La t dei passi e' una mistura di normali: ogni giorno ha il
#      suo moltiplicatore di varianza, estratto dalla gamma condizionata;
#   2. delta dalla sua normale condizionata (la t e' una mistura di normali
#      con pesi lambda), poi uno spostamento comune di livello fra mu e delta,
#      che la verosimiglianza non distingue e che il passo 1 muoverebbe troppo
#      piano;
#   3. lambda dalla sua gamma condizionata, omega e sigma dalla condizionata
#      valutata su una griglia.
# Le serie sono indipendenti fra loro, ma i passi sono vettorizzati su tutte.
#
# Uso:
#   fit <- media_sondaggi(d, fine = as.Date("2026-11-19"))
#   d: data.frame con colonne serie, istituto, data (Date), y (percentuale),
#      n (campione, NA ammesso)
#   fit$media     data, serie, media, lo90, hi90, lo50, hi50, sd
#   fit$sondaggi  i sondaggi con peso, effetto istituto e valore corretto
#   fit$effetti   serie, istituto, sondaggi, effetto, lo90, hi90
#   fit$parametri serie, tau, sigma, omega (mediana e 90%), rhat
#   fit$finale    matrice estrazioni x serie dell'ultimo giorno
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(parallel) })

.rhat <- function(m) {                 # m: iterazioni x catene (split R-hat)
  n <- nrow(m) %/% 2
  m <- cbind(m[1:n, , drop = FALSE], m[(n + 1):(2 * n), , drop = FALSE])
  W <- mean(apply(m, 2, var)); B <- n * var(colMeans(m))
  if (!is.finite(W) || W == 0) return(NA_real_)
  sqrt(((n - 1) / n * W + B / n) / W)
}

NU <- 4          # gradi di liberta' della t degli effetti istituto

.catena <- function(o, n_iter, n_warm, thin, seed, prior, gl_passi) {
  set.seed(seed)
  Tn <- o$Tn; S <- o$S; N <- length(o$y); H <- o$H
  # W[t, s]: moltiplicatore della varianza del passo fra t-1 e t (1 se i passi
  # sono normali; con passi t di Student e' estratto a ogni iterazione)
  W <- matrix(1, Tn, S)

  # filtro in avanti, vettorizzato sulle serie. P e B: precisione e
  # precisione x valore delle osservazioni aggregate per (giorno, serie)
  filtro <- function(P, B, tau2) {
    M <- C <- matrix(0, Tn, S); ll <- numeric(S)
    m <- o$m0; c0 <- rep(100, S)
    for (t in seq_len(Tn)) {
      if (t > 1) c0 <- c0 + tau2 * W[t, ]
      p <- P[t, ]
      if (any(p > 0)) {
        k <- p > 0
        v <- 1 / p[k] + c0[k]
        ll[k] <- ll[k] - 0.5 * (log(v) + (B[t, k] / p[k] - m[k])^2 / v)
        cn <- 1 / (1 / c0 + p)
        m <- cn * (m / c0 + B[t, ]); c0 <- cn
      }
      M[t, ] <- m; C[t, ] <- c0
    }
    list(M = M, C = C, ll = ll)
  }
  indietro <- function(f, tau2) {
    mu <- matrix(0, Tn, S)
    mu[Tn, ] <- f$M[Tn, ] + sqrt(f$C[Tn, ]) * rnorm(S)
    for (t in (Tn - 1):1) {
      g <- f$C[t, ] / (f$C[t, ] + tau2 * W[t + 1, ])
      mu[t, ] <- f$M[t, ] + g * (mu[t + 1, ] - f$M[t, ]) +
        sqrt(f$C[t, ] * (1 - g)) * rnorm(S)
    }
    mu
  }
  lprior_tau <- function(tau) -tau^2 / (2 * prior$tau^2) + log(tau)  # su log(tau)

  tau <- rep(0.08, S); sigma <- rep(1, S); omega <- rep(1, S)
  delta <- numeric(H); lambda <- rep(1, H)
  passo <- rep(0.5, S); acc <- numeric(S)
  g_sigma <- seq(0.01, 6, length.out = 150)
  g_omega <- seq(0.02, 6, length.out = 200)
  lp_sigma <- -g_sigma^2 / (2 * prior$sigma^2)
  lp_omega <- -g_omega^2 / (2 * prior$omega^2)
  se2_g <- outer(o$se2, g_sigma^2, `+`)            # N x griglia
  lse2_g <- log(se2_g)

  tieni <- seq(n_warm + thin, n_iter, by = thin)
  out_mu <- array(0, c(length(tieni), Tn, S))
  out_par <- array(0, c(length(tieni), S, 3))
  out_delta <- matrix(0, length(tieni), H)
  cella <- o$cella; ucella <- o$ucella
  aggrega <- function(x) {
    v <- numeric(Tn * S); v[ucella] <- rowsum(x, cella)[, 1]; matrix(v, Tn, S)
  }

  for (it in seq_len(n_iter)) {
    # 1. tau (Metropolis sulla marginale) e mu (FFBS)
    prec <- o$w / (o$se2 + sigma[o$s]^2)
    P <- aggrega(prec); B <- aggrega(prec * (o$y - delta[o$h]))
    f0 <- filtro(P, B, tau^2)
    tau_p <- tau * exp(passo * rnorm(S))
    f1 <- filtro(P, B, tau_p^2)
    ok <- log(runif(S)) < f1$ll + lprior_tau(tau_p) - f0$ll - lprior_tau(tau)
    ok[is.na(ok)] <- FALSE
    if (any(ok)) {
      tau[ok] <- tau_p[ok]
      f0$M[, ok] <- f1$M[, ok]; f0$C[, ok] <- f1$C[, ok]
    }
    if (it <= n_warm) {                              # taratura del passo
      acc <- acc + ok
      if (it %% 50 == 0) {
        passo <- passo * exp((acc / 50 - 0.35)); acc[] <- 0
      }
    }
    mu <- indietro(f0, tau^2)
    if (is.finite(gl_passi)) {
      # passi t di Student: pesi lambda dalla gamma condizionata
      passi2 <- (mu[-1, , drop = FALSE] - mu[-Tn, , drop = FALSE])^2
      lam <- rgamma(length(passi2), (gl_passi + 1) / 2,
                    (gl_passi + passi2 / rep(tau^2, each = Tn - 1)) / 2)
      W[-1, ] <- 1 / lam
    }
    mu_i <- mu[cbind(o$t, o$s)]

    # 2. effetti istituto
    if (H > 0) {
      r <- o$y - mu_i
      ph <- rowsum(prec, o$h)[, 1] + lambda / omega[o$sh]^2
      delta <- rowsum(prec * r, o$h)[, 1] / ph + rnorm(H) / sqrt(ph)
      delta[o$solo] <- 0                             # serie con un solo istituto

      # spostamento comune: mu + c, delta - c lascia intatta la verosimiglianza;
      # c si estrae dalla sua condizionata (prior di delta e di mu(1))
      pc <- rowsum(lambda, o$sh)[, 1] / omega[o$serie_h]^2 + 0.01
      mc <- (rowsum(lambda * delta, o$sh)[, 1] / omega[o$serie_h]^2 +
               0.01 * (o$m0[o$serie_h] - mu[1, o$serie_h])) / pc
      cs <- numeric(S)
      cs[o$serie_h] <- mc + rnorm(length(pc)) / sqrt(pc)
      cs[o$serie_sole] <- 0
      mu <- mu + rep(cs, each = Tn)
      mu_i <- mu_i + cs[o$s]
      delta <- delta - cs[o$sh]
      delta[o$solo] <- 0

      # 3a. pesi della t e omega dalla griglia
      lambda <- rgamma(H, (NU + 1) / 2, (NU + delta^2 / omega[o$sh]^2) / 2)
      ss <- rowsum(lambda * delta^2, o$sh)[, 1]
      lp <- -outer(o$J, log(g_omega)) - outer(ss / 2, 1 / g_omega^2) +
        rep(lp_omega, each = length(ss))
      for (k in seq_along(ss)) {
        pr <- exp(lp[k, ] - max(lp[k, ]))
        omega[o$serie_h[k]] <- sample(g_omega, 1, prob = pr)
      }
    }

    # 3b. sigma dalla griglia
    e2 <- (o$y - mu_i - delta[o$h])^2
    lp <- rowsum(o$w * (-0.5 * lse2_g - 0.5 * e2 / se2_g), o$s)
    for (s in seq_len(S)) {
      pr <- lp[s, ] + lp_sigma; pr <- exp(pr - max(pr))
      sigma[s] <- sample(g_sigma, 1, prob = pr)
    }

    k <- match(it, tieni)
    if (!is.na(k)) {
      out_mu[k, , ] <- mu
      out_par[k, , ] <- cbind(tau, sigma, omega)
      out_delta[k, ] <- delta
    }
  }
  list(mu = out_mu, par = out_par, delta = out_delta)
}

media_sondaggi <- function(d, fine, inizio = min(d$data), n_iter = 4000,
                           n_warm = 1000, thin = 5, n_catene = 4,
                           seed = 20261120, n_max = 5000, finestra = 7,
                           esponente = 0.5, gl_passi = 2,
                           prior = list(tau = 0.25, sigma = 2, omega = 2)) {
  d <- as.data.table(copy(d))
  d <- d[data >= inizio & data <= fine & !is.na(y)]
  setorder(d, serie, data, istituto)
  giorni <- seq(inizio, fine, by = "day")
  nomi_serie <- unique(d$serie)
  d[, n := as.numeric(n)]

  # campione mancante: quello mediano; oltre n_max non aggiunge precisione
  n_mediano <- median(d$n, na.rm = TRUE)
  d[is.na(n), n := if (is.na(n_mediano)) 1000 else n_mediano]
  d[, se2 := { p <- pmin(pmax(y, 1), 99) / 100; p * (1 - p) / pmin(n, n_max) * 1e4 }]
  # peso: 1 / k^esponente, con k = sondaggi dello stesso istituto entro
  # +-finestra giorni
  d[, peso := {
    dd <- as.integer(data)
    1 / vapply(dd, function(x) sum(abs(dd - x) <= finestra), 0)^esponente
  }, by = .(serie, istituto)]

  d[, s := match(serie, nomi_serie)]
  d[, t := match(data, giorni)]
  d[, h := .GRP, by = .(s, istituto)]
  hs <- d[, .(np = .N), by = .(h, s, istituto)][order(h)]
  hs[, J := .N, by = s]
  cella <- (d$s - 1L) * length(giorni) + d$t

  o <- list(Tn = length(giorni), S = length(nomi_serie), H = nrow(hs),
            y = d$y, se2 = d$se2, w = d$peso, s = d$s, t = d$t, h = d$h,
            cella = cella, ucella = sort(unique(cella)),
            sh = hs$s, solo = hs$J == 1,
            serie_h = sort(unique(hs$s)),
            serie_sole = unique(hs[J == 1, s]),
            J = hs[, .N, by = s][order(s)]$N,
            m0 = d[, .(m = mean(y[data <= min(data) + 30])), by = s][order(s)]$m)

  semi <- seed + seq_len(n_catene)
  catene <- mclapply(semi, function(sd) .catena(o, n_iter, n_warm, thin, sd, prior, gl_passi),
                     mc.cores = min(n_catene, detectCores()))
  errori <- vapply(catene, inherits, TRUE, "try-error")
  if (any(errori)) stop(catene[[which(errori)[1]]])

  nk <- dim(catene[[1]]$mu)[1]
  mu <- array(unlist(lapply(catene, `[[`, "mu")), c(nk, o$Tn, o$S, n_catene))
  par <- array(unlist(lapply(catene, `[[`, "par")), c(nk, o$S, 3, n_catene))
  del <- array(unlist(lapply(catene, `[[`, "delta")), c(nk, o$H, n_catene))

  qs <- function(x, p) quantile(x, p, names = FALSE)
  media <- rbindlist(lapply(seq_along(nomi_serie), function(s) {
    m <- matrix(aperm(mu[, , s, , drop = FALSE], c(1, 4, 2, 3)), ncol = o$Tn)
    data.table(data = giorni, serie = nomi_serie[s], media = colMeans(m),
               lo90 = apply(m, 2, qs, 0.05), hi90 = apply(m, 2, qs, 0.95),
               lo50 = apply(m, 2, qs, 0.25), hi50 = apply(m, 2, qs, 0.75),
               sd = apply(m, 2, sd))
  }))
  # ogni serie parte dal suo primo sondaggio
  primo <- d[, .(primo = min(data)), by = serie]
  media <- media[primo, on = "serie"][data >= primo][, primo := NULL]

  hs[, serie := nomi_serie[s]]
  dm <- matrix(aperm(del, c(1, 3, 2)), ncol = o$H)
  hs[, `:=`(effetto = colMeans(dm), lo90 = apply(dm, 2, qs, 0.05),
            hi90 = apply(dm, 2, qs, 0.95))]
  d[, effetto := hs$effetto[h]]
  d[, corretto := y - effetto]

  nomi_par <- c("tau", "sigma", "omega")
  parametri <- rbindlist(lapply(seq_along(nomi_serie), function(s) {
    r <- lapply(1:3, function(k) {
      x <- par[, s, k, ]
      setNames(list(median(x), qs(x, 0.05), qs(x, 0.95), .rhat(x)),
               paste0(nomi_par[k], c("", "_lo", "_hi", "_rhat")))
    })
    ult <- mu[, o$Tn, s, ]
    as.data.table(c(list(serie = nomi_serie[s]), unlist(r, recursive = FALSE),
                    list(rhat_finale = .rhat(ult))))
  }))

  finale <- matrix(aperm(mu[, o$Tn, , , drop = FALSE], c(1, 4, 3, 2)), ncol = o$S,
                   dimnames = list(NULL, nomi_serie))

  list(media = media,
       sondaggi = d[, .(serie, istituto, data, y, n, peso, effetto, corretto)],
       effetti = hs[, .(serie, istituto, sondaggi = np, effetto, lo90, hi90)],
       parametri = parametri, finale = finale, giorni = giorni)
}
