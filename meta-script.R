library(tidyverse)
library(metafor)
library(readxl)
library(writexl)
library(here)
library(glue)
library(ratlas)

dat <- read_xlsx(here("data/Meta_Data_Analysis_A1c.xlsx"))

dat <- dat %>% 
  mutate(label = glue("{First_Author} ({Publication_Year})"))

## Data preparation ------------------------------------------------------------
# binary recording 0 = No, 1 = Yes
already_calc <- dat %>% 
  rename(hedges_g = `Hedges’_g`) %>% 
  filter(!is.na(hedges_g)) %>% 
  filter(!(Study_ID %in% c(37, 38))) %>% 
  select(label,
         pilot_full = "Pilot or Full Trial",
         tech_based = "Technology_based",
         intervention_type = "Intervention_Type",
         increase_smbg = "Increase_SMBG",
         intervention_t1d_appt = "Intervention_T1D_Appt",
         type_control = "Control_Group",
         intervention_target = "Intervention_target",
         intervention_length = "Intervention_Length_Mon",
         intervention_time = "Intervention_Time_Hour",
         BL_TX_N:FU_Con_A1c_SD,
         hedges_g,
         sampling_var = v) %>% 
  mutate(BL_TX_N = as.numeric(BL_TX_N),
         BL_Con_N = as.numeric(BL_Con_N),
         BL_TX_A1c_SD = as.numeric(BL_TX_A1c_SD),
         BL_Con_A1c_SD = as.numeric(BL_Con_A1c_SD),
         FU_TX_N = as.numeric(FU_TX_N),
         FU_Con_N = as.numeric(FU_Con_N),
         FU_TX_A1c_SD = as.numeric(FU_TX_A1c_SD),
         FU_Con_A1c_SD = as.numeric(FU_Con_A1c_SD),
         FU_TX_A1c_M = as.numeric(FU_TX_A1c_M),
         BL_TX_A1c_M = as.numeric(BL_TX_A1c_M),
         FU_Con_A1c_M = as.numeric(FU_Con_A1c_M),
         BL_Con_A1c_M = as.numeric(BL_Con_A1c_M),
         intervention_time = as.numeric(intervention_time),
         tech_based = case_when(tech_based == "No" ~ 0,
                                tech_based == "Yes" ~ 1),
         increase_smbg = case_when(increase_smbg == "No" ~ 0,
                                   increase_smbg == "Yes" ~ 1),
         intervention_t1d_appt = case_when(intervention_t1d_appt == "No" ~ 0,
                                           intervention_t1d_appt == "Yes" ~
                                             1))

dat <- dat %>% 
  select(label,
         pilot_full = "Pilot or Full Trial",
         tech_based = "Technology_based",
         intervention_type = "Intervention_Type",
         increase_smbg = "Increase_SMBG",
         intervention_t1d_appt = "Intervention_T1D_Appt",
         type_control = "Control_Group",
         intervention_target = "Intervention_target",
         intervention_length = "Intervention_Length_Mon",
         intervention_time = "Intervention_Time_Hour",
         BL_TX_N:FU_Con_A1c_SD) %>% 
  mutate(BL_TX_N = as.numeric(BL_TX_N),
         BL_Con_N = as.numeric(BL_Con_N),
         BL_TX_A1c_SD = as.numeric(BL_TX_A1c_SD),
         BL_Con_A1c_SD = as.numeric(BL_Con_A1c_SD),
         FU_TX_N = as.numeric(FU_TX_N),
         FU_Con_N = as.numeric(FU_Con_N),
         FU_TX_A1c_SD = as.numeric(FU_TX_A1c_SD),
         FU_Con_A1c_SD = as.numeric(FU_Con_A1c_SD),
         FU_TX_A1c_M = as.numeric(FU_TX_A1c_M),
         BL_TX_A1c_M = as.numeric(BL_TX_A1c_M),
         FU_Con_A1c_M = as.numeric(FU_Con_A1c_M),
         BL_Con_A1c_M = as.numeric(BL_Con_A1c_M),
         intervention_time = as.numeric(intervention_time),
         tech_based = case_when(tech_based == "No" ~ 0,
                                tech_based == "Yes" ~ 1),
         increase_smbg = case_when(increase_smbg == "No" ~ 0,
                                   increase_smbg == "Yes" ~ 1),
         intervention_t1d_appt = case_when(intervention_t1d_appt == "No" ~ 0,
                                           intervention_t1d_appt == "Yes" ~
                                             1)) %>% 
  anti_join(already_calc) %>% 
  filter(!is.na(BL_TX_A1c_SD)) %>% 
  filter(!is.na(BL_Con_A1c_SD)) %>% 
  filter(!is.na(BL_TX_N)) %>% 
  filter(!is.na(BL_Con_N)) %>% 
  filter(!is.na(BL_TX_A1c_M)) %>% 
  filter(!is.na(BL_Con_A1c_M)) %>% 
  filter(!is.na(FU_TX_A1c_M)) %>% 
  filter(!is.na(FU_Con_A1c_M)) %>% 
  filter(!is.na(BL_TX_N)) %>% 
  filter(!is.na(BL_Con_N)) %>% 
  filter(!is.na(FU_TX_N)) %>% 
  filter(!is.na(FU_Con_N))

## Calculate ES ----------------------------------------------------------------
dat <- dat %>% 
  mutate(pre_pooled_sd = sqrt((((BL_TX_N - 1) * BL_TX_A1c_SD^2) +
                             ((BL_Con_N - 1) * BL_Con_A1c_SD^2)) /
                            (BL_TX_N + BL_Con_N - 2)),
         tx_mean_diff = FU_TX_A1c_M - BL_TX_A1c_M,
         con_mean_diff = FU_Con_A1c_M - BL_Con_A1c_M,
         mean_diff = tx_mean_diff - con_mean_diff,
         es = mean_diff / pre_pooled_sd,
         N = FU_TX_N + FU_Con_N,
         hedges_g = es * (1 - (3 / (4 * N - 9))),
         sampling_var = ((FU_TX_N + FU_Con_N) / (FU_TX_N * FU_Con_N)) +
           (hedges_g^2 / (2 * (FU_TX_N + FU_Con_N))),
         sampling_var = sampling_var * (1 - (3 / (4 * N - 9))) *
           (1 - (3 / (4 * N - 9))))

to_analyze <- dat %>% 
  filter(!is.na(hedges_g)) %>% 
  bind_rows(already_calc %>%
              mutate(hedges_g = as.numeric(hedges_g),
                     sampling_var = as.numeric(sampling_var))) %>% 
  mutate(hedges_g = -1 * hedges_g)

## Primary analysis ------------------------------------------------------------
### Overall
metafor::rma(data = to_analyze, yi = hedges_g, vi = sampling_var)

overall_meta <- metafor::rma(data = to_analyze, yi = hedges_g, vi = sampling_var)
forest(overall_meta, slab = to_analyze$label, cex = .7)

regtest(overall_meta, predictor = "vi")
# z = 3.2555, p = .0011 (indicates risk of publication bias)
trimfill(overall_meta)
# indicates 13 studies likely missing on left side (likely consequence of publication bias)

### Pilot vs Full Trial
full_trial <- to_analyze %>% 
  filter(pilot_full == "Full Trial")
pilot <- to_analyze %>% 
  filter(pilot_full == "Pilot")

metafor::rma(data = pilot, yi = hedges_g, vi = sampling_var)

metafor::rma(data = full_trial, yi = hedges_g, vi = sampling_var)

pilot_meta <- metafor::rma(data = pilot %>% 
                             arrange(desc(hedges_g)),
                           yi = hedges_g, vi = sampling_var)
forest(pilot_meta,
       slab = pilot %>% 
         arrange(desc(hedges_g)) %>% 
         pull(label),
       cex = 1)
ggsave2(filename = here("output/pilot-forest.png"), dir = "h",
        bg = "white")

full_trial_meta <- metafor::rma(data = full_trial %>% 
                                  arrange(desc(hedges_g)),
                                yi = hedges_g, vi = sampling_var)
forest(full_trial_meta,
       slab = full_trial %>% 
         arrange(desc(hedges_g)) %>% 
         pull(label),
       cex = .9)

regtest(pilot_meta, predictor = "vi")
# z = -0.2666, p = .7898 (publication bias not likely)
trimfill(pilot_meta)
# estimates 1 study likely missing on right side (i.e., underestimation of effect size)

regtest(full_trial_meta, predictor = "vi")
# z = 2.8609, p = .0042 (publication bias likely)
trimfill(full_trial_meta)
# estimates 11 studies likely missing on left side (consistent with publication bias)

## Secondary analysis ----------------------------------------------------------
### Meta-analyses --------------------------------------------------------------
direct <- to_analyze %>% 
  filter(intervention_type == "Direct")
indirect <- to_analyze %>% 
  filter(intervention_type == "Indirect")
combined <- to_analyze %>% 
  filter(intervention_type == "Combined")
tech_yes <- to_analyze %>% 
  filter(tech_based == 1)
tech_no <- to_analyze %>% 
  filter(tech_based == 0)
increase_smbg_yes <- to_analyze %>% 
  filter(increase_smbg == 1)
increase_smbg_no <- to_analyze %>% 
  filter(increase_smbg == 0)
intervention_appt_yes <- to_analyze %>% 
  filter(intervention_t1d_appt == 1)
intervention_appt_no <- to_analyze %>% 
  filter(intervention_t1d_appt == 0)
intervention_target_child <- to_analyze %>% 
  filter(intervention_target == "Child-only")
intervention_target_parent <- to_analyze %>% 
  filter(intervention_target == "Parent-only")
intervention_target_family <- to_analyze %>% 
  filter(intervention_target == "Family")
type_con_uc <- to_analyze %>%
  filter(type_control == "Usual Care")
type_con_wl <- to_analyze %>%
  filter(type_control == "Waitlist Control")
type_con_edu <- to_analyze %>%
  filter(type_control == "Education")
type_con_att <- to_analyze %>%
  filter(type_control == "Atttention-control")
type_con_oth <- to_analyze %>%
  filter(type_control == "Other Intervention")


missing_dat <- dat %>% 
  filter(is.na(hedges_g))

metafor::rma(data = direct, yi = hedges_g, vi = sampling_var)
metafor::rma(data = indirect, yi = hedges_g, vi = sampling_var)
metafor::rma(data = combined, yi = hedges_g, vi = sampling_var)

metafor::rma(data = tech_yes, yi = hedges_g, vi = sampling_var)
metafor::rma(data = tech_no, yi = hedges_g, vi = sampling_var)

metafor::rma(data = increase_smbg_yes, yi = hedges_g, vi = sampling_var)
metafor::rma(data = increase_smbg_no, yi = hedges_g, vi = sampling_var)

metafor::rma(data = intervention_appt_yes, yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_appt_no, yi = hedges_g, vi = sampling_var)

metafor::rma(data = intervention_target_child, yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_target_parent, yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_target_family, yi = hedges_g, vi = sampling_var)

metafor::rma(data = type_con_uc, yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_wl, yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_edu, yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_att, yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_oth, yi = hedges_g, vi = sampling_var)


metafor::rma(data = direct %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = indirect %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = combined %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

metafor::rma(data = tech_yes %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = tech_no %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

metafor::rma(data = increase_smbg_yes %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = increase_smbg_no %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

metafor::rma(data = intervention_appt_yes %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_appt_no %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

metafor::rma(data = intervention_target_child %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_target_parent %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = intervention_target_family %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

metafor::rma(data = type_con_uc %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_wl %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_edu %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_att %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)
metafor::rma(data = type_con_oth %>% 
               filter(pilot_full == "Full Trial"),
             yi = hedges_g, vi = sampling_var)

### Meta-regression ------------------------------------------------------------
metafor::rma(data = to_analyze, yi = hedges_g, vi = sampling_var,
             mods = ~ intervention_length)

metafor::rma(data = to_analyze, yi = hedges_g, vi = sampling_var,
             mods = ~ intervention_time)
