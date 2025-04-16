library(dplyr)
library(gtsummary)
library(rio)
library(forcats)
library(here)


#Load the data and remove observations with no visit dates
data <- import(
    here("Aim 1 & 2 - TB Men Masterdataset (analysis) 19 April 2023.dta")) %>%
  filter(!(record_id %in%  c(57, 537, 810)))

data <- data %>%
  mutate(
  gender_cat = factor(gender, levels = 1:2, labels = c("Male", "Female")),
  relat_status = cut(s1_q3, breaks = c(-Inf, 1, 2, 3, Inf), labels = c(
    "Single", "Relationship, unmarried", "Married", "Seperated/Divorced/Widowed")),
  race_cat = factor(s1_q1, levels = c(1:4, 99), labels = c(
    "Other", "Black", "Other", "Coloured", "Other" )),
  born_SA = factor(s1_q2, levels = c(0:1), labels = c("No", "Yes")),
  edu_level = cut(s1_q4, breaks = c(-Inf, 2, 4, Inf), labels = c(
    "Primary and below", "Grade 8 -11 (before Matric)", "Matric and above")),
  liv_condition = factor(s1_q5, levels = c(1:3, 99), labels = c(
    "Informal dwelling", "Formal house", "Formal house", "Formal house")),
  liv_condition = fct_explicit_na(liv_condition, "Missing"),
  live_alone_2weeks = factor(s1_q6, levels = 1:2, labels = c("Yes", "No")),
  live_with = factor(s1_q7, levels = 1:3, labels = c(
    "Family or Partner", "Living with friends", " Living with strangers")),
  live_with = fct_explicit_na(live_with, "Missing"),
  num_adults = s1_q8,
  num_child = s1_q9,
  bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
  income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
    "< 2000", "2000 - 5000", "> 5000")),
  ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
    "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
  know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2), labels = c("Yes", "No")),
  hiv_status = cut(s2_q4_hiv_status, breaks = c(-Inf, 1, 2, Inf), labels = c(
    "Positive", "Negative", "Unknown")),
  depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
    "Non-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderate-Severe (15-19)", "Severe (20-27)")),
  depression = fct_explicit_na(depression, "Missing"),
  anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
    "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
  anxiety = fct_explicit_na(anxiety, "Missing"),
  alcohol_use = fct_explicit_na(cut(aa_q1a_en, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes")), "Missing"),
  tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
  tb_knowledge = fct_explicit_na(tb_knowledge, "Missing"),
  social_capital = fct_explicit_na(factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")), "Missing"),
  tb_rel_stigma = fct_explicit_na(factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")), "Missing"),
  track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
  )


data %>% select(gender_cat, calculated_age, relat_status, race_cat, edu_level,
                liv_condition, live_alone_2weeks, num_adults,
                num_child, bread_winner, income, ever_had_tb,
                know_any_tb, hiv_status, depression, anxiety,
                alcohol_use, tb_knowledge, social_capital, tb_rel_stigma, track_comp) %>%
  tbl_summary(by = gender_cat, type = all_dichotomous() ~ "categorical",
              label = list(
                gender_cat = "Gender",
                calculated_age = "Age",
                relat_status = "Relationship status",
                race_cat = "Race",
                born_SA = "Born in SA",
                edu_level = "Level of education",
                liv_condition = "Living condition",
                live_alone_2weeks ="For the past 2 weeks, have you lived alone?",
                live_with = "Who do you live with?",
                num_adults = "For the past 2 weeks, # adults in household",
                num_child = "For the past 2 weeks, # children in household",
                bread_winner = "Primary breadwinner",
                income = "Total household income per month",
                ever_had_tb = "Ever had TB before",
                know_any_tb = "Know anyone who currently has TB",
                hiv_status = "HIV status",
                depression = "Depression (PHQ-9)",
                anxiety = "Anxiety (GAD-7)",
                alcohol_use = "Alcohol use",
                tb_knowledge = "TB Knowledge",
                social_capital = "Social capital",
                tb_rel_stigma = "TB Related Stigma",
                track_comp = "Finished treatment"
              )) %>%
add_overall() %>%
  add_p()

%>%
  as_gt() %>%
  gt::gtsave(filename = "Table1.docx")

