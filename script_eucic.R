# datasets EUCIC
# resistentieanalyse dataset --------------
library(openssl)
monster_afgenomen <- data_MO %>%
  select(Monsternummer, GlimsId = patient_id, MonsterAfgenomen) %>%
  mutate(MonsterAfgenomen = date(MonsterAfgenomen)) %>%
  distinct()

ic_opname <- opnames %>%
  filter(year(`Opname Startdatum`) >= 2015,
         `Opname Type` != "Dagbehandeling") %>%
  filter(grepl("Intensive",Afdeling)) %>%
  left_join(monster_afgenomen) %>%
  filter(!is.na(Monsternummer)) %>%
  mutate(`Verblijf Startdatum` = date(`Verblijf Startdatum`),
         `Verblijf Einddatum` = date(`Verblijf Einddatum`)) %>%
  filter(MonsterAfgenomen >= `Verblijf Startdatum` & MonsterAfgenomen <= `Verblijf Einddatum`) %>%
  select(Monsternummer) %>%
  mutate(ICU = TRUE) %>%
  distinct()

eucic <- data_MO %>%
  mutate(Leeftijd = round(time_length(difftime(MonsterAfgenomen, PatientGeboortedatum), "years"),0)) %>%
  mutate(patient_anoniem = openssl::sha256(patient_id, key = "hieriets")) %>%
  filter(MonsterMateriaal == "Urine" | MonsterMateriaal == "Wond" | MonsterMateriaal == "Bloed") %>%
  mutate(afname_new = as.POSIXct(paste0(Jaar, "-01-01")) + lubridate::days(sample(0:364, nrow(.), TRUE))) %>%
  left_join(ic_opname) %>%
  select(patient_anoniem, PatientGeslacht, Leeftijd, afname_new, MonsterMateriaal, Aanvragersgroep2, Organisme, ICU, ESBL_l, ESBL_rsi, BRMO_l, 52:130)

write.csv2(eucic, "output/eucic_isolaten.csv", row.names = FALSE, na = "")

# dataset aureus bact ---------------
aureus_bact <- data_MO %>%
  filter(genus_species == "Staphylococcus aureus") %>%
  filter(MonsterMateriaal == "Bloed") %>%
  mutate(MonsterAfgenomen = date(MonsterAfgenomen)) %>%
  select(Monsternummer, MonsterAfgenomen, genus_species) %>%
  distinct()

bloedkweekresultaten_eucic <- bloedkweekresultaten_clean %>%
  mutate(patient_anoniem = openssl::sha256(patient_id, key = "hieriets")) %>%
  select(patient_anoniem, Monsternummer, AfnameDatum, Uitslag) %>%
  left_join(aureus_bact, by = "Monsternummer") %>%
  group_by(patient_anoniem) %>%
  filter(any(genus_species == "Staphylococcus aureus")) %>%
  ungroup() %>%
  distinct(patient_anoniem, AfnameDatum, genus_species, .keep_all = TRUE)

write.csv2(bloedkweekresultaten_eucic, "output/bloedkweekresultaten_eucic.csv", row.names = FALSE, na = "")

