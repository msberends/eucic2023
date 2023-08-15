library(tidyverse)

certe <- read_csv2("staph_eucic.csv")

# - casusvrije dagen: 5
# - Alleen eerste episode voor elke patient
# 
# patient id
# status (0 = dood, 1 = leeft nog)
# days (tot laatste positieve)
# age_group (per 10 jaar)

test <- certe |> filter(patient_anoniem == "00c007ce9550e2454acd439ba58a8715960f8415e6a7cb961155baf5c2bf9a41")

# test |>
km <- certe |>
  group_by(patient_anoniem) |> 
  # sorteren nodig voor episodebepaling
  arrange(afname_new) |>
  # episode los bepalen voor positief EN negatief
  group_by(patient_anoniem, STAU_positive) |>
  mutate(episode = get_episode(afname_new, case_free_days = 5)) |>
  # weer terug naar groepering op patient
  group_by(patient_anoniem) |> 
  # variabele maken om te testen of negatieve voorafgegaan werd door positieve
  mutate(neg_after_pos = STAU_positive == FALSE & lag(STAU_positive) == TRUE) |>
  # alleen positieven uit episode 1 houden, of als het een negatieve was die na een positieve kwam
  filter(episode == 1 & STAU_positive == TRUE | neg_after_pos == TRUE) |>
  # alleen eerste negatieve na positieve houden (anders kun je een negatieve na episode 3 overhouden)
  # filter(STAU_positive == TRUE | !duplicated(neg_after_pos)) |> 
  # tabel maken om over te houden
  transmute(patient_anoniem,
            status = as.double(any(neg_after_pos, na.rm = TRUE)), # 1: eindigt met negatieve, 0: eindigt met positieve
            days = as.double(difftime(max(afname_new[STAU_positive == TRUE]), min(afname_new[STAU_positive == TRUE]), units = "days")),
            age_group = age_groups(Leeftijd, split_at = c(67)),
            gender = PatientGeslacht) |> 
  # groepering er nu uit
  ungroup() |> 
  rename(patient_id = patient_anoniem) |> 
  distinct()

fit <- survfit(Surv(time = days, event = ifelse(status == 0, 1, 0)) ~ age_group, data = km)
fit <- survfit(Surv(time = days, event = ifelse(status == 0, 1, 0)) ~ gender, data = km)
ggsurvplot(fit, 
                        data = km,
                        size = 0.5,                 # change line size
                       # ylab = "Proportie non R meropenem",
                        # palette =
                        #   c("#0100a4", "#a3004f"),# custom color palettes
                        conf.int = FALSE,          # Add confidence interval
                        pval = TRUE,              # Add p-value
                        risk.table = TRUE,        # Add risk table
                        risk.table.col = "strata",# Risk table color by groups
                        # legend.labs = levels(km$age_group), # Change legend labels
                        risk.table.height = 0.25, # Useful to change when you have multiple groups
                        ggtheme = theme_bw(),
                        cumevents = TRUE # Change ggplot2 theme
)


