suppressMessages({
  library(tidyverse); library(showtext); library(treemapify)
})

dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto(); showtext_opts(dpi = 300)

COL_ROSSO <- "#F12938"; COL_GIALLO <- "#F2A900"; COL_BLU <- "#0478EA"
COL_NERO  <- "#1C1C1C"; COL_VIOLA <- "#A82DE3"; COL_VERDE <- "#1B9E77"
CAP <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"
pct <- function(x) paste0(format(round(x,1), nsmall=1, decimal.mark=","), "%")
mig <- function(x) format(round(x), big.mark=".", decimal.mark=",")

raw <- readRDS(file.path(dir_in,"gov_10a_exp_raw.rds"))
geo_lab <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="Media UE")

## ===========================================================================
## GRAFICO 1 — TREEMAP composizione spesa pubblica italiana (per famiglie)
## ===========================================================================
it <- raw %>% filter(geo=="IT", na_item=="TE", TIME_PERIOD==2024, unit=="MIO_EUR")
mio <- function(c) { v <- it$values[it$cofog99==c]; if(length(v)==0) 0 else v }
tot <- mio("TOTAL")

tm <- tribble(
  ~area,                  ~voce,                     ~val,                              ~colore,
  "Protezione sociale",   "Pensioni di vecchiaia",   mio("GF1002"),                     "#F12938",
  "Protezione sociale",   "Reversibilità",           mio("GF1003"),                     "#F4626E",
  "Protezione sociale",   "Malattia e invalidità",   mio("GF1001"),                     "#F78A93",
  "Protezione sociale",   "Famiglia",                mio("GF1004"),                     "#FAA9AF",
  "Protezione sociale",   "Disoccupazione",          mio("GF1005"),                     "#FCC9CD",
  "Protezione sociale",   "Altra protez. sociale",   mio("GF10")-mio("GF1002")-mio("GF1003")-mio("GF1001")-mio("GF1004")-mio("GF1005"), "#FDE0E2",
  "Interessi sul debito", "Interessi sul debito",    mio("GF0107"),                     "#F2A900",
  "Sanità e istruzione",  "Sanità",                  mio("GF07"),                       "#0478EA",
  "Sanità e istruzione",  "Istruzione",              mio("GF09"),                       "#6BA8E8",
  "Funzioni dello Stato", "Funzionamento Stato",     mio("GF01")-mio("GF0107"),         "#A82DE3",
  "Funzioni dello Stato", "Ordine pubblico",         mio("GF03"),                       "#C162E8",
  "Funzioni dello Stato", "Difesa",                  mio("GF02"),                       "#D8A6F2",
  "Economia e territorio","Affari economici",        mio("GF04"),                       "#1B9E77",
  "Economia e territorio","Ambiente, cultura, case", mio("GF05")+mio("GF08")+mio("GF06"), "#7FCBB0"
) %>%
  mutate(quota = val/tot*100,
         txtcol = ifelse(colore %in% c("#F12938","#F4626E","#0478EA","#6BA8E8","#1B9E77","#A82DE3","#C162E8","#F2A900"), "white", COL_NERO),
         lab = paste0(voce, "\n", pct(quota)))

p1 <- ggplot(tm, aes(area=val, fill=colore, subgroup=area)) +
  geom_treemap(colour="white", size=1.2, start="topleft") +
  geom_treemap_subgroup_border(colour="white", size=5, start="topleft") +
  geom_treemap_text(aes(label=lab, colour=txtcol), start="topleft",
                    place="topleft", reflow=TRUE, min.size=4, size=9,
                    family="Source Sans Pro", fontface="bold",
                    padding.x=grid::unit(1.4,"mm"), padding.y=grid::unit(1.4,"mm")) +
  scale_fill_identity() + scale_colour_identity() +
  labs(title = "Dove va la spesa pubblica italiana",
       subtitle = "Composizione per funzione e principali voci, quota sulla spesa totale, Italia, 2024",
       caption = CAP) +
  theme_minimal() +
  theme(text = element_text(family="Source Sans Pro"),
        plot.title = element_text(family="Source Sans Pro SemiBold", size=14, color=COL_NERO, margin=margin(b=0.1, unit="cm")),
        plot.subtitle = element_text(size=9, color=COL_NERO, margin=margin(b=0.3, unit="cm")),
        plot.caption = element_text(size=9, color=COL_NERO, hjust=1, margin=margin(t=0.3, unit="cm")),
        plot.title.position = "plot",
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid=element_blank(), plot.margin=unit(c(.4,.4,.4,.4),"cm"))
ggsave(file.path(dir_out,"01_treemap_composizione.png"), p1, width=9, height=6.8, dpi=220, bg="white")
cat("[ok] 01_treemap_composizione.png\n")

## ===========================================================================
## GRAFICO 2 — STACKED BAR pensioni + interessi + resto, asse X nascosto
## ===========================================================================
g2 <- raw %>% filter(TIME_PERIOD==2024, na_item=="TE", unit=="PC_TOT",
                     cofog99 %in% c("GF1002","GF1003","GF0107")) %>%
  mutate(voce=ifelse(cofog99=="GF0107","int","pen")) %>%
  group_by(geo, voce) %>% summarise(v=sum(values), .groups="drop") %>%
  pivot_wider(names_from=voce, values_from=v) %>%
  mutate(paese=geo_lab[geo], resto=100-pen-int, rigida=pen+int,
         int_dentro = int >= 5)
ord <- g2 %>% arrange(rigida) %>% pull(paese)
g2 <- g2 %>% mutate(paese=factor(paese, levels=ord))

g2l <- g2 %>% select(paese, Pensioni=pen, Interessi=int, `Resto della spesa`=resto) %>%
  pivot_longer(-paese, names_to="voce", values_to="v") %>%
  mutate(voce=factor(voce, levels=c("Pensioni","Interessi","Resto della spesa")))
pal2 <- c("Pensioni"=COL_ROSSO, "Interessi"=COL_GIALLO, "Resto della spesa"="#C9CDD1")

p2 <- ggplot(g2l, aes(x=paese, y=v, fill=voce)) +
  geom_col(width=0.72, position=position_stack(reverse=TRUE)) +
  # pensioni: dentro il rosso
  geom_text(data=g2, aes(x=paese, y=pen/2, label=pct(pen)), inherit.aes=FALSE,
            colour="white", family="Source Sans Pro", fontface="bold", size=3.3) +
  # interessi dentro (Italia, Spagna)
  geom_text(data=filter(g2, int_dentro), aes(x=paese, y=pen+int/2, label=pct(int)),
            inherit.aes=FALSE, colour=COL_NERO, family="Source Sans Pro", fontface="bold", size=3.3) +
  # interessi fuori a destra, in arancione (Francia, Media UE, Germania)
  geom_text(data=filter(g2, !int_dentro), aes(x=paese, y=pen+int+1.2, label=pct(int)),
            inherit.aes=FALSE, colour="#C77F00", hjust=0, family="Source Sans Pro", fontface="bold", size=3.3) +
  # resto: spostato a destra dentro il grigio
  geom_text(data=g2, aes(x=paese, y=pen+int+resto*0.6, label=pct(resto)), inherit.aes=FALSE,
            colour=COL_NERO, family="Source Sans Pro", size=3.2) +
  scale_fill_manual(values=pal2) +
  scale_y_continuous(expand=expansion(mult=c(0,0.04)), breaks=NULL) +
  coord_flip(clip="off") +
  labs(title = "In Italia pensioni e interessi pesano più che altrove",
       subtitle = "Quota della spesa pubblica totale assorbita da pensioni e interessi sul debito, 2024",
       caption = CAP) +
  theme_minimal() +
  theme(text=element_text(family="Source Sans Pro"),
        legend.position="top", legend.title=element_blank(),
        legend.text=element_text(size=10, color=COL_NERO),
        axis.title=element_blank(),
        axis.text.y=element_text(size=9, color=COL_NERO),
        axis.text.x=element_blank(), axis.line.x=element_blank(), axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.title.position="plot",
        plot.title=element_text(family="Source Sans Pro SemiBold", size=14, color=COL_NERO, margin=margin(b=0.1,unit="cm")),
        plot.subtitle=element_text(size=9, color=COL_NERO, margin=margin(b=0.3,unit="cm")),
        plot.caption=element_text(size=9, color=COL_NERO, hjust=1, margin=margin(t=0.4,unit="cm")),
        plot.margin=unit(c(.4,.6,.4,.4),"cm"))
ggsave(file.path(dir_out,"02_pensioni_interessi.png"), p2, width=8.2, height=5, dpi=220, bg="white")
cat("[ok] 02_pensioni_interessi.png\n")

## ===========================================================================
## GRAFICO 3 — spesa pro-capite PPS, due versioni a facet (paesi sull'asse X)
## ===========================================================================
pop <- readRDS(file.path(dir_in,"demo_gind_raw.rds")) %>%
  filter(indic_de=="AVG", TIME_PERIOD==2024) %>% select(geo, pop=values)
ppp <- readRDS(file.path(dir_in,"prc_ppp_ind_raw.rds")) %>%
  filter(na_item=="PPP_EU27_2020", ppp_cat=="GDP", TIME_PERIOD==2024) %>% select(geo, ppp=values)
short <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="UE")
voci3 <- c(GF07="Sanità", GF09="Istruzione", GF1002="Pensioni di vecchiaia")
g3 <- raw %>% filter(TIME_PERIOD==2024, na_item=="TE", unit=="MIO_EUR", cofog99 %in% names(voci3)) %>%
  left_join(pop, by="geo") %>% left_join(ppp, by="geo") %>%
  mutate(pps = values*1e6/pop/ppp,
         paese = factor(short[geo], levels=c("Italia","Francia","Germania","Spagna","UE")),
         voce = factor(voci3[cofog99], levels=c("Sanità","Istruzione","Pensioni di vecchiaia")))
pal3 <- c("Italia"=COL_ROSSO,"Francia"=COL_VIOLA,"Germania"=COL_NERO,"Spagna"=COL_GIALLO,"UE"=COL_BLU)

base3 <- function() list(
  geom_col(width=0.82, show.legend=FALSE),
  facet_wrap(~voce, nrow=1),
  scale_fill_manual(values=pal3),
  labs(title = "In sanità e scuola l'Italia spende meno per abitante",
       subtitle = "Spesa pubblica per abitante in standard di potere d'acquisto (PPS), a prezzi reali, 2024",
       caption = CAP)
)
theme3 <- function(...) theme_minimal() + theme(
  text=element_text(family="Source Sans Pro"),
  axis.title=element_blank(),
  axis.text.x=element_text(size=9, color=COL_NERO, angle=40, hjust=1),
  strip.text=element_text(size=11.5, color=COL_NERO, face="bold"),
  panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
  panel.spacing=unit(0.8,"cm"),
  plot.title.position="plot",
  plot.title=element_text(family="Source Sans Pro SemiBold", size=14, color=COL_NERO, margin=margin(b=0.1,unit="cm")),
  plot.subtitle=element_text(size=9, color=COL_NERO, margin=margin(b=0.3,unit="cm")),
  plot.caption=element_text(size=9, color=COL_NERO, hjust=1, margin=margin(t=0.4,unit="cm")),
  plot.margin=unit(c(.4,.4,.4,.4),"cm"), ...)

# versione 1: BAR CHART orizzontale, con label di valore, facet per voce
p3a <- ggplot(g3, aes(x=pps, y=fct_rev(paese), fill=paese)) +
  geom_col(width=0.78, show.legend=FALSE) +
  geom_text(aes(label=mig(pps)), hjust=-0.15, family="Source Sans Pro", size=3.4,
            fontface="bold", color=COL_NERO) +
  facet_wrap(~voce, nrow=1) +
  scale_fill_manual(values=pal3) +
  scale_x_continuous(breaks=NULL, expand=expansion(mult=c(0,0.22))) +
  labs(title = "In sanità e scuola l'Italia spende meno per abitante",
       subtitle = "Spesa pubblica per abitante in standard di potere d'acquisto (PPS), a prezzi reali, 2024",
       caption = CAP) +
  theme_minimal() +
  theme(text=element_text(family="Source Sans Pro"),
        axis.title=element_blank(),
        axis.text.y=element_text(size=9, color=COL_NERO),
        axis.text.x=element_blank(), axis.ticks=element_blank(),
        strip.text=element_text(size=11.5, color=COL_NERO, face="bold"),
        panel.grid=element_blank(), panel.spacing=unit(0.8,"cm"),
        plot.title.position="plot",
        plot.title=element_text(family="Source Sans Pro SemiBold", size=14, color=COL_NERO, margin=margin(b=0.1,unit="cm")),
        plot.subtitle=element_text(size=9, color=COL_NERO, margin=margin(b=0.3,unit="cm")),
        plot.caption=element_text(size=9, color=COL_NERO, hjust=1, margin=margin(t=0.4,unit="cm")),
        plot.margin=unit(c(.4,.4,.4,.4),"cm"))
ggsave(file.path(dir_out,"03_procapite_pps_v1.png"), p3a, width=10, height=5.2, dpi=220, bg="white")
cat("[ok] 03_procapite_pps_v1.png\n")

# versione 2: asse Y nascosto, valori sopra le barre
p3b <- ggplot(g3, aes(x=paese, y=pps, fill=paese)) + base3() +
  geom_text(aes(label=mig(pps)), vjust=-0.5, family="Source Sans Pro", size=3.4,
            fontface="bold", color=COL_NERO) +
  scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
  theme3(axis.text.y=element_blank(), panel.grid.major.y=element_blank())
ggsave(file.path(dir_out,"03_procapite_pps_v2.png"), p3b, width=10, height=5.4, dpi=220, bg="white")
cat("[ok] 03_procapite_pps_v2.png\n")

if (file.exists(file.path(dir_out,"03_procapite_pps.png"))) file.remove(file.path(dir_out,"03_procapite_pps.png"))
cat("[done]\n")
