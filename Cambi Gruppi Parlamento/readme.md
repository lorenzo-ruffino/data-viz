# Cambri gruppi in Parlamento

## Senato

https://dati.senato.it/DatiSenato/browse/6?testo_generico=11&legislatura=19

Bisogna scegliere la legislatura, andare sulla voce "Gruppi", poi "Variazioni dei gruppi nel periodo", scegliere il formato e fare download.

## Camera

https://dati.camera.it/sparql

Query (per i dati delle varie legislature, bisogna cambiare il filtro):

```SELECT DISTINCT
  ?gruppo
  ?nomeGruppo
  ?deputato
  ?nome
  ?cognome
  ?inizio
  ?fine
  ?legislatura
WHERE {
  # Persona collegata a un mandato di Camera
  ?deputato a foaf:Person ;
            ocd:rif_mandatoCamera ?mandato .

  # Deputato della XVIII legislatura
  ?d a ocd:deputato ;
     ocd:rif_mandatoCamera ?mandato ;
     ocd:rif_leg ?legislatura ;
     ocd:aderisce ?aderisce ;
     foaf:firstName ?nome ;
     foaf:surname ?cognome .
  FILTER(?legislatura = <http://dati.camera.it/ocd/legislatura.rdf/repubblica_19>)

  # Gruppo parlamentare (tutti, non solo attuali)
  ?aderisce ocd:rif_gruppoParlamentare ?gruppo .
  ?gruppo <http://purl.org/dc/terms/alternative> ?sigla ;
          dc:title ?nomeGruppo .

  # Estremi dell’adesione al gruppo
  OPTIONAL { ?aderisce ocd:startDate ?inizio . }
  OPTIONAL { ?aderisce ocd:endDate ?fine . }
}
ORDER BY ?deputato ?inizio
```

*Dati delle XXIX legislatura aggiornati al 23 agosto 2025*