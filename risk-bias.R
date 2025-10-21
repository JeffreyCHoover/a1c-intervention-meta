library(tidyverse)
library(here)
library(glue)
library(readxl)
library(writexl)

dat <- read_xlsx(here("data/RoB Assessment.xlsx"))
col_labels <- names(dat)
col_labels <- str_replace(col_labels, "\\.", "_")
names(dat) <- col_labels
dat <- dat |> 
  rename("study_id" = "Study ID",
         "first_author" = "First Author (Last name)",
         "year" = "Publication Year") |> 
  filter(study_id != "Study ID")

rob_output <-  dat |> 
  select("study_id", "first_author", "year")

# check answer formatting
dat |> 
  select(starts_with("Q")) |> 
  pivot_longer(cols = everything(), names_to = "item_id", values_to = "score") |> 
  distinct(score)

# appears to be correct
# Domain 1
d1 <- dat |> 
  select("study_id", "first_author", "year", starts_with("Q1")) |> 
  mutate(d1_risk = case_when(Q1_2 %in% c("No", "Probably No") ~ "High risk",
                             Q1_2 == "No Information" & 
                               Q1_3 %in% c("Yes", "Probably Yes") ~ "High risk",
                             Q1_2 %in% c("Yes", "Probably Yes") &
                               Q1_1 %in% c("Yes", "Probably Yes", "No Information") &
                               Q1_3 %in% c("No", "Probably No", "No Information") ~
                               "Low risk",
                             TRUE ~ "Some concerns")) |> 
  select("study_id", "first_author", "year", "d1_risk")

# appears to be issues (e.g., Yi-Frazier 2024)
# Domain 2
d2 <- dat |> 
  select("study_id", "first_author", "year", starts_with("Q2")) |> 
  mutate(d2_p1_risk = case_when(Q2_1 %in% c("No", "Probably No") &
                                  Q2_2 %in% c("No", "Probably No") ~ "Low risk",
                                (Q2_1 %in% c("Yes", "Probably Yes", "No Information") |
                                  Q2_2 %in% c("Yes", "Probably Yes", "No Information")) &
                                  Q2_3 %in% c("No", "Probably No") ~ "Low risk",
                                (Q2_1 %in% c("Yes", "Probably Yes", "No Information") |
                                  Q2_2 %in% c("Yes", "Probably Yes", "No Information")) &
                                  Q2_3 %in% c("Yes", "Probably Yes") &
                                  Q2_4 %in% c("Yes", "Probably Yes", "No Information") &
                                  Q2_5 %in% c("No", "Probably No", "No Information") ~ "High risk",
                                TRUE ~ "Some concerns"),
         d2_p2_risk = case_when(Q2_6 %in% c("Yes", "Probably Yes") ~ "Low risk",
                                Q2_6 %in% c("No", "Probably No", "No Information") &
                                  Q2_7 %in% c("No", "Probably No") ~ "Some concerns",
                                TRUE ~ "High risk"),
         d2_risk = case_when(d2_p1_risk == "Low risk" &
                               d2_p2_risk == "Low risk" ~ "Low risk",
                             d2_p1_risk == "High risk" |
                               d2_p2_risk == "High risk" ~ "High risk",
                             TRUE ~ "Some concerns")) |>                 
  select("study_id", "first_author", "year", "d2_risk")

# appears to be correct
# Domain 3
d3 <- dat |> 
  select("study_id", "first_author", "year", starts_with("Q3")) |> 
  mutate(d3_risk = case_when(Q3_1 %in% c("Yes", "Probably Yes") ~ "Low risk",
                             Q3_1 %in% c("No", "Probably No", "No Information") &
                               Q3_2 %in% c("Yes", "Probably Yes") ~ "Low risk",
                             Q3_1 %in% c("No", "Probably No", "No Information") &
                               Q3_2 %in% c("No", "Probably No") &
                               Q3_3 %in% c("No", "Probably No") ~ "Low risk",
                             Q3_1 %in% c("No", "Probably No", "No Information") &
                               Q3_2 %in% c("No", "Probably No") &
                               Q3_3 %in% c("Yes", "Probably Yes", "No Information") &
                               Q3_4 %in% c("No", "Probably No") ~ "Some concerns",
                             TRUE ~ "High risk")) |> 
  select("study_id", "first_author", "year", "d3_risk")

# appears to be issues (e.g., Hilliard 2020)
# Domain 4
d4 <- dat |> 
  select("study_id", "first_author", "year", starts_with("Q4")) |> 
  mutate(d4_risk = case_when(Q4_1 %in% c("Yes", "Probably Yes") ~ "High risk",
                             Q4_1 %in% c("No", "Probably No", "No Information") &
                               Q4_2 %in% c("Yes", "Probably Yes") ~ "High risk",
                             Q4_1 %in% c("No", "Probably No", "No Information") &
                               Q4_2 == "No Information" &
                               Q4_3 %in% c("Yes", "Probably Yes", "No Information") &
                               Q4_4 %in% c("Yes", "Probably Yes", "No Information") & 
                               Q4_5 %in% c("Yes", "Probably Yes", "No Information") ~ "High risk",
                             Q4_1 %in% c("No", "Probably No", "No Information") &
                               Q4_2 %in% c("No", "Probably No") &
                               Q4_3 %in% c("Yes", "Probably Yes", "No Information") &
                               Q4_4 %in% c("Yes", "Probably Yes", "No Information") & 
                               Q4_5 %in% c("Yes", "Probably Yes", "No Information") ~ "High risk",
                             Q4_1 %in% c("No", "Probably No", "No Information") &
                               Q4_2 %in% c("No", "Probably No") &
                               Q4_3 %in% c("No", "Probably No") ~ "Low risk",
                             Q4_1 %in% c("No", "Probably No", "No Information") &
                               Q4_2 %in% c("No", "Probably No") &
                               Q4_3 %in% c("Yes", "Probably Yes", "No Information") &
                               Q4_4 %in% c("No", "Probably No") ~ "Low risk",
                             TRUE ~ "Some concerns")) |> 
  select("study_id", "first_author", "year", "d4_risk")

# Looks correct
# Domain 5
d5 <- dat |> 
  select("study_id", "first_author", "year", starts_with("Q5")) |> 
  mutate(d5_risk = case_when(Q5_2 %in% c("Yes", "Probably Yes") |
                               Q5_3 %in% c("Yes", "Probably Yes") ~ "High risk",
                             Q5_2 %in% c("No", "Probably No") &
                               Q5_3 %in% c("No", "Probably No") &
                               Q5_1 %in% c("Yes", "Probably Yes") ~ "Low risk",
                             TRUE ~ "Some concerns")) |> 
  select("study_id", "first_author", "year", "d5_risk")

final_rob <- left_join(d1, d2, by = c("study_id", "first_author", "year")) |> 
  left_join(d3, by = c("study_id", "first_author", "year")) |> 
  left_join(d4, by = c("study_id", "first_author", "year")) |> 
  left_join(d5, by = c("study_id", "first_author", "year")) |> 
  mutate(overall_risk = case_when(d1_risk == "Low risk" &
                                    d2_risk == "Low risk" &
                                    d3_risk == "Low risk" &
                                    d4_risk == "Low risk" &
                                    d5_risk == "Low risk" ~ "Low risk",
                                  d1_risk == "High risk" |
                                    d2_risk == "High risk" |
                                    d3_risk == "High risk" |
                                    d4_risk == "High risk" |
                                    d5_risk == "High risk" ~ "High risk",
                                  TRUE ~ "Some concerns")) |> 
  select(-"study_id") |> 
  mutate(study = str_c(first_author, ", ", year)) |> 
  select("study", starts_with("d"), "overall_risk")

write_xlsx(final_rob, path = here("output/risk-of-bias.xlsx"))
