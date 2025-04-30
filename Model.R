library(nnet)
library(dplyr)
library(gt)
library(gtsummary)
library(rio)
library(broom)
library(tidyr)
library(mice)

#Model for men and women combined
data <- import("Model data all.csv") %>%
  mutate(# create
    gender_cat = factor(gender.x, levels = 1:2, labels = c("Male", "Female")),
    relat_status = cut(s1_q3, breaks = c(-Inf, 1, 2, 3, Inf), labels = c(
      "Single", "Relationship, unmarried", "Married", "Seperated/Divorced/Widowed")),
    race_cat = factor(s1_q1.x, levels = c(1:4, 99), labels = c(
      "White", "Black", "Indian", "Coloured", "Other" )),
    born_SA = factor(s1_q2, levels = c(0:1), labels = c("No", "Yes")),
    edu_level = cut(s1_q4, breaks = c(-Inf, 2, 4, Inf), labels = c(
      "Primary and below", "Grade 8 -11 (before Matric)", "Matric and above")),
    edu_level2 = cut(s1_q4, breaks = c(-Inf, 2, 3, Inf),
                     labels = c("No secondary", "Matric", "Above matric")),
    liv_condition = factor(s1_q5, levels = c(1:3, 99), labels = c(
      "Informal dwelling", "Formal house", "Formal house", "Formal house")),
    live_alone_2weeks = factor(s1_q6, levels = 1:2, labels = c("Yes", "No")),
    live_with = factor(s1_q7, levels = 1:3, labels = c(
      "Family or Partner", "Living with friends", " Living with strangers")),
    num_adults = s1_q8,
    num_child = s1_q9,
    bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
    income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
    "< 2000", "2000 - 5000", "> 5000")),
    ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
      "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
    know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2), labels = c("Yes", "No")),
    hiv_status = factor(s2_q4_hiv_status, levels = 1:4, labels = c(
      "Positive", "Negative", "Didn't disclose", "I don't know")),
    depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
      "Non-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderate-Severe (15-19)", "Severe (20-27)")),
    anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
      "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
    ptsd = factor(ptsd20_cat5, levels = 0:4, labels = c("Low", "1", "2", "3", "High")),
    alcohol_use = cut(aa_q1a_en, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes")),
    tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
    social_capital = factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")),
    tb_rel_stigma = factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")),
    track_comp = factor(track_comp.x, levels = 0:1, labels = c("No", "Yes")),
    social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
    social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
    social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
    social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
    depression_score = rowSums(across(matches("^dep_.+_en$")), na.rm = TRUE),
    gender_norms = rowSums(across(matches("^gn_.+_en$")), na.rm = TRUE),
    alcohol_use = rowSums(across(matches("^aa_q.+_en$")), na.rm = TRUE),
    patient_satisfaction = rowSums(across(matches("^pc_q.+_en$")), na.rm = TRUE)
  )

#create social support scales
data <- data %>%
  mutate(social_support_all_cat =
           case_when(social_support_all <3 ~ "low support",
                     social_support_all  >=3 & social_support_all <= 5 ~ "moderate support",
                     social_support_all > 5 ~ "high support"),
         social_support_family_cat =
           case_when(social_support_family <3 ~ "low support",
                     social_support_family  >=3 & social_support_family <= 5 ~ "moderate support",
                     social_support_family > 5 ~ "high support"),
         social_support_friends_cat =
           case_when(social_support_friends <3 ~ "low support",
                     social_support_friends  >=3 & social_support_friends <= 5 ~ "moderate support",
                     social_support_friends > 5 ~ "high support"),
         social_support_sig_cat =
           case_when(social_support_sig_other <3 ~ "low support",
                     social_support_sig_other  >=3 & social_support_sig_other <= 5 ~ "moderate support",
                     social_support_sig_other > 5 ~ "high support")
         )

#Models

#Overall
model_data <- dplyr::select(data, class, gender_cat , calculated_age.x  ,
                       edu_level2 , income , depression_score ,
                       ever_had_tb ,  hiv_status , social_support_all_cat ,
                       social_support_family_cat, social_support_friends_cat,
                       social_support_sig_cat,
                       live_alone_2weeks)

imp <- mice(model_data)
fit <- with(imp, multinom(class ~  gender_cat + calculated_age.x  +
                   edu_level2 + income +
                   ever_had_tb +  hiv_status + social_support_all_cat +
                   live_alone_2weeks))

cbind(summary(pool(fit))[,1:2], round(summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)[,3:9], 3)) %>%
  select(y.level, term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(3:6, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = "y.level", values_from = c(
    "estimate", "p.value", "conf_int")) %>%
  select(term, ends_with("_2"), ends_with("_3")) %>%
  mutate(estimate_2_ci = paste0(estimate_2, " (", conf_int_2, ")"),
         estimate_3_ci = paste0(estimate_3, " (", conf_int_3, ")")) %>%
  select(term, estimate_2_ci, p.value_2, estimate_3_ci, p.value_3) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "lightpink")),
            locations = cells_body(
              columns = p.value_2,
              rows = p.value_2 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "lightblue")),
            locations = cells_body(
              columns = p.value_3,
              rows = p.value_3 < 0.05))

confint(summary(pool(fit)))


model_data$class_2 <- ifelse(model_data$class == 1, 0, 1)

mod_2 <- glm(class_2 ~  gender_cat + calculated_age.x  +
               edu_level2 + income + scale(depression_score) +
               ever_had_tb +  hiv_status + social_support +
               live_alone_2weeks, data = model_data,
             family = "binomial")

#Model for men only
data_men <- import("Model data men.csv") %>%
  mutate(# create
    gender_cat = factor(gender.x, levels = 1:2, labels = c("Male", "Female")),
    relat_status = cut(s1_q3, breaks = c(-Inf, 1, 2, 3, Inf), labels = c(
      "Single", "Relationship, unmarried", "Married", "Seperated/Divorced/Widowed")),
    race_cat = factor(s1_q1.x, levels = c(1:4, 99), labels = c(
      "White", "Black", "Indian", "Coloured", "Other" )),
    born_SA = factor(s1_q2, levels = c(0:1), labels = c("No", "Yes")),
    edu_level = cut(s1_q4, breaks = c(-Inf, 2, 4, Inf), labels = c(
      "Primary and below", "Grade 8 -11 (before Matric)", "Matric and above")),
    edu_level2 = cut(s1_q4, breaks = c(-Inf, 2, 3, Inf),
                     labels = c("No secondary", "Matric", "Above matric")),
    liv_condition = factor(s1_q5, levels = c(1:3, 99), labels = c(
      "Informal dwelling", "Formal house", "Formal house", "Formal house")),
    live_alone_2weeks = factor(s1_q6, levels = 1:2, labels = c("Yes", "No")),
    live_with = factor(s1_q7, levels = 1:3, labels = c(
      "Family or Partner", "Living with friends", " Living with strangers")),
    num_adults = s1_q8,
    num_child = s1_q9,
    bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
    income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
      "< 2000", "2000 - 5000", "> 5000")),
    ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
      "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
    know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2), labels = c("Yes", "No")),
    hiv_status = factor(s2_q4_hiv_status, levels = 1:4, labels = c(
      "Positive", "Negative", "Didn't disclose", "I don't know")),
    depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
      "Non-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderate-Severe (15-19)", "Severe (20-27)")),
    anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
      "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
    ptsd = factor(ptsd20_cat5, levels = 0:4, labels = c("Low", "1", "2", "3", "High")),
    alcohol_use = cut(aa_q1a_en, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes")),
    tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
    social_capital = factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")),
    tb_rel_stigma = factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")),
    track_comp = factor(track_comp.x, levels = 0:1, labels = c("No", "Yes")),
    social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
    social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
    social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
    social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
    depression_score = rowSums(across(matches("^dep_.+_en$")), na.rm = TRUE),
    gender_norms = rowSums(across(matches("^gn_.+_en$")), na.rm = TRUE),
    alcohol_use = rowSums(across(matches("^aa_q.+_en$")), na.rm = TRUE),
    patient_satisfaction = rowSums(across(matches("^pc_q.+_en$")), na.rm = TRUE)
  )

#create social support scales
data_men <- data_men %>%
  mutate(social_support_all_cat =
           case_when(social_support_all <3 ~ "low support",
                     social_support_all  >=3 & social_support_all <= 5 ~ "moderate support",
                     social_support_all > 5 ~ "high support"),
         social_support_family_cat =
           case_when(social_support_family <3 ~ "low support",
                     social_support_family  >=3 & social_support_family <= 5 ~ "moderate support",
                     social_support_family > 5 ~ "high support"),
         social_support_friends_cat =
           case_when(social_support_friends <3 ~ "low support",
                     social_support_friends  >=3 & social_support_friends <= 5 ~ "moderate support",
                     social_support_friends > 5 ~ "high support"),
         social_support_sig_cat =
           case_when(social_support_sig_other <3 ~ "low support",
                     social_support_sig_other  >=3 & social_support_sig_other <= 5 ~ "moderate support",
                     social_support_sig_other > 5 ~ "high support")
  )

model_data_men <- dplyr::select(data_men, class, gender_cat , calculated_age.x  ,
                            edu_level2 , income , depression_score , relat_status,
                            ever_had_tb ,  hiv_status , social_support_all_cat ,
                            social_support_family_cat, social_support_friends_cat,
                            social_support_sig_cat, liv_condition,
                            live_alone_2weeks)

imp <- mice(model_data_men)
fit <- with(imp, multinom(class ~   calculated_age.x  + relat_status +
                            edu_level2 + income + liv_condition +
                            ever_had_tb +  hiv_status + social_support_all_cat +
                            live_alone_2weeks))

cbind(summary(pool(fit))[,1:2], round(summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)[,3:9], 3)) %>%
  select(y.level, term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(3:6, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = "y.level", values_from = c(
    "estimate", "p.value", "conf_int")) %>%
  select(term, ends_with("_2"), ends_with("_3")) %>%
  mutate(estimate_2_ci = paste0(estimate_2, " (", conf_int_2, ")"),
         estimate_3_ci = paste0(estimate_3, " (", conf_int_3, ")")) %>%
  select(term, estimate_2_ci, p.value_2, estimate_3_ci, p.value_3) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "lightpink")),
            locations = cells_body(
              columns = p.value_2,
              rows = p.value_2 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "lightblue")),
            locations = cells_body(
              columns = p.value_3,
              rows = p.value_3 < 0.05))














#Model for women only
data_women <- import("Model data women.csv") %>%
  mutate(# create
    gender_cat = factor(gender.x, levels = 1:2, labels = c("Male", "Female")),
    relat_status = cut(s1_q3, breaks = c(-Inf, 1, 2, 3, Inf), labels = c(
      "Single", "Relationship, unmarried", "Married", "Seperated/Divorced/Widowed")),
    race_cat = factor(s1_q1.x, levels = c(1:4, 99), labels = c(
      "White", "Black", "Indian", "Coloured", "Other" )),
    born_SA = factor(s1_q2, levels = c(0:1), labels = c("No", "Yes")),
    edu_level = cut(s1_q4, breaks = c(-Inf, 2, 4, Inf), labels = c(
      "Primary and below", "Grade 8 -11 (before Matric)", "Matric and above")),
    edu_level2 = cut(s1_q4, breaks = c(-Inf, 2, 3, Inf),
                     labels = c("No secondary", "Matric", "Above matric")),
    liv_condition = factor(s1_q5, levels = c(1:3, 99), labels = c(
      "Informal dwelling", "Formal house", "Formal house", "Formal house")),
    live_alone_2weeks = factor(s1_q6, levels = 1:2, labels = c("Yes", "No")),
    live_with = factor(s1_q7, levels = 1:3, labels = c(
      "Family or Partner", "Living with friends", " Living with strangers")),
    num_adults = s1_q8,
    num_child = s1_q9,
    bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
    income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
      "< 2000", "2000 - 5000", "> 5000")),
    ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
      "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
    know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2), labels = c("Yes", "No")),
    hiv_status = factor(s2_q4_hiv_status, levels = 1:4, labels = c(
      "Positive", "Negative", "Didn't disclose", "I don't know")),
    depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
      "Non-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderate-Severe (15-19)", "Severe (20-27)")),
    anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
      "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
    ptsd = factor(ptsd20_cat5, levels = 0:4, labels = c("Low", "1", "2", "3", "High")),
    alcohol_use = cut(aa_q1a_en, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes")),
    tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
    social_capital = factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")),
    tb_rel_stigma = factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")),
    track_comp = factor(track_comp.x, levels = 0:1, labels = c("No", "Yes")),
    social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
    social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
    social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
    social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
    depression_score = rowSums(across(matches("^dep_.+_en$")), na.rm = TRUE),
    gender_norms = rowSums(across(matches("^gn_.+_en$")), na.rm = TRUE),
    alcohol_use = rowSums(across(matches("^aa_q.+_en$")), na.rm = TRUE),
    patient_satisfaction = rowSums(across(matches("^pc_q.+_en$")), na.rm = TRUE)
  )

#create social support scales
data_women <- data_women %>%
  mutate(social_support_all_cat =
           case_when(social_support_all <3 ~ "low support",
                     social_support_all  >=3 & social_support_all <= 5 ~ "moderate support",
                     social_support_all > 5 ~ "high support"),
         social_support_family_cat =
           case_when(social_support_family <3 ~ "low support",
                     social_support_family  >=3 & social_support_family <= 5 ~ "moderate support",
                     social_support_family > 5 ~ "high support"),
         social_support_friends_cat =
           case_when(social_support_friends <3 ~ "low support",
                     social_support_friends  >=3 & social_support_friends <= 5 ~ "moderate support",
                     social_support_friends > 5 ~ "high support"),
         social_support_sig_cat =
           case_when(social_support_sig_other <3 ~ "low support",
                     social_support_sig_other  >=3 & social_support_sig_other <= 5 ~ "moderate support",
                     social_support_sig_other > 5 ~ "high support")
  )

model_data_women <- dplyr::select(data_women, class, calculated_age.x  ,
                            edu_level2 , income , depression_score , relat_status,
                            ever_had_tb ,  hiv_status , social_support_all_cat ,
                            social_support_family_cat, social_support_friends_cat,
                            social_support_sig_cat, tb_knowledge,
                            live_alone_2weeks)

imp <- mice(model_data_women)
fit <- with(imp, glm(class ~  calculated_age.x  + tb_knowledge + relat_status +
                            edu_level2 + income +
                            ever_had_tb +  hiv_status + social_support_all_cat +
                            live_alone_2weeks))

summ <- summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)

cbind(summ[, 1, drop = FALSE],round(summ[, c(2, 6:8)], 2)) %>%
  select(term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(3:4, \(x) round(x, digits = 3)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  select(-conf.low, -conf.high) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "lightpink")),
            locations = cells_body(
              columns = p.value,
              rows = p.value < 0.05))

round(summary(pool(fit), conf.int = TRUE, exponentiate = TRUE), 3)
