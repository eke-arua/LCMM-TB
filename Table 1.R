library(dplyr)
library(gtsummary)
library(rio)
library(forcats)
library(here)


#Load the data and remove observations with no visit dates
# data <- import(
#     here("Aim 1 & 2 - TB Men Masterdataset (analysis) 19 April 2023.dta")) %>%
#   filter(!(record_id %in%  c(57, 537, 810)))

data <- import(
  here("Model data all.csv"))


data <- data %>%
  mutate(
  gender_cat = factor(gender, levels = 1:2, labels = c("Male", "Female")),
  relat_status = cut(s1_q3, breaks = c(-Inf, 1, Inf), labels = c(
    "Not in a relationship", "In a relationshp")),
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
  liv_wth_child = cut(s1_q9, breaks = c(-Inf, 0, Inf),
                      labels = c("None", "One or more")),
  liv_wth_child = fct_explicit_na(liv_wth_child, "Missing"),
  bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
  gender_norms = rowSums(across(matches("^gn_.+_en$")), na.rm = FALSE)/10,
  girl_equity = rowSums(across(matches("^eg_.+_en$")), na.rm = FALSE)/4,
  income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
    "< 2000", "2000 - 5000", "> 5000")),
  employment_status = cut(s1_q18, breaks = c(-Inf, 2, Inf),
                          labels = c("Employed (earning income)", "Unemployed")),
  ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
    "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
  know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2),
                       labels = c("Yes", "No")),
  hiv_status = real_hiv_status,
  hiv_status = case_when( is.na(hiv_status) ~ s2_q4_hiv_status,
                          .default = hiv_status),
  hiv_status = cut(hiv_status, breaks = c(-Inf, 1, 2, Inf), labels = c(
    "Positive", "Negative", "Unknown")),
  depression = cut(phq9_sum, breaks = c(-Inf, 5, 7, 14, Inf), labels = c(
    "None-Minimal (0-4)", "Mild (5-7)", "Moderate (7-14)",
    "Moderate-Severe (> 15)")),
  depression = fct_explicit_na(depression, "Missing"),
  medical_mistrust = cut(medical_misstrust, breaks = quantile(medical_misstrust, probs = c(0, 1/3, 2/3, 1), na.rm = T),
                         labels = c("Low", "Medium", "High")),
  medical_mistrust = fct_explicit_na(medical_mistrust, "Missing"),
  anxiety = cut(gad7_sum, breaks = c(-Inf, 4, 10, 14, Inf), labels = c(
    "Mild (0-4)", "Moderate (4-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
  anxiety = fct_explicit_na(anxiety, "Missing"),
  alcohol = rowSums(across(matches("^aa_.+_en$")), na.rm = FALSE),
  alcohol = if_else(aa_q1a_en == 0, 0, alcohol),
  alcohol_use = cut(alcohol,
                    breaks = c(-Inf, 7, 14, Inf),
                    labels = c("Low-risk", "Hazardous/Harmful", "Alcohol Dependent")),
  alcohol_use = fct_explicit_na(alcohol_use, "Missing"),
  tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
  tb_knowledge = fct_explicit_na(tb_knowledge, "Missing"),
  social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
  social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
  social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
  social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
  social_capital = fct_explicit_na(factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")), "Missing"),
  tb_rel_stigma = fct_explicit_na(factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")), "Missing"),
  hiv_stigma = hiv_stigma_sum,
  track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
  )

#create social support scales
data <- data %>%
  mutate(social_support_all_cat =
           case_when(social_support_all <3 ~ "low support",
                     social_support_all  >=3 & social_support_all <= 5 ~ "moderate support",
                      social_support_all > 5 ~ "high support") )
#,
  #        social_support_family_cat =
  #          case_when(social_support_family <3 ~ "low support",
  #                    social_support_family  >=3 & social_support_family <= 5 ~ "moderate support",
  #                    social_support_family > 5 ~ "high support"),
  #        social_support_friends_cat =
  #          case_when(social_support_friends <3 ~ "low support",
  #                    social_support_friends  >=3 & social_support_friends <= 5 ~ "moderate support",
  #                    social_support_friends > 5 ~ "high support"),
  #        social_support_sig_cat =
  #          case_when(social_support_sig_other <3 ~ "low support",
  #                    social_support_sig_other  >=3 & social_support_sig_other <= 5 ~ "moderate support",
  #                    social_support_sig_other > 5 ~ "high support")



#create table
data %>% select(gender_cat, calculated_age, relat_status, race_cat, edu_level,
                liv_condition, live_alone_2weeks, liv_wth_child, #income,
                bread_winner, ever_had_tb, employment_status,
                know_any_tb, hiv_status, depression, anxiety, medical_mistrust,
                gender_norms, girl_equity,
                alcohol_use, tb_knowledge, social_capital, tb_rel_stigma, hiv_stigma, track_comp) %>%
  tbl_summary(by = gender_cat, type = all_dichotomous() ~ "categorical",
              label = list(
                liv_wth_child = "Live with children",
                gender_cat = "Gender",
                calculated_age = "Age",
                relat_status = "Relationship status",
                employment_status = "Employment status",
                race_cat = "Race",
                born_SA = "Born in SA",
                edu_level = "Level of education",
                liv_condition = "Living condition",
                live_alone_2weeks ="For the past 2 weeks, have you lived alone?",
                live_with = "Who do you live with?",
                num_adults = "For the past 2 weeks, # adults in household",
                bread_winner = "Primary breadwinner",
                #income = "Total household income per month",
                ever_had_tb = "Ever had TB before",
                know_any_tb = "Know anyone who currently has TB",
                hiv_status = "HIV status",
                depression = "Depression (PHQ-9)",
                anxiety = "Anxiety (GAD-7)",
                gender_norms = "Gender norms score",
                medical_mistrust = "Medical mistrust",
                girl_equity = "Equity for girls score",
                alcohol_use = "Alcohol use",
                tb_knowledge = "TB Knowledge",
                social_capital = "Social capital",
                tb_rel_stigma = "TB Related Stigma",
                hiv_stigma = "HIV stigma",
                track_comp = "Finished treatment"
              )) %>%
add_overall() %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(filename = "Table1.docx")



