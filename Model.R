library(nnet)
library(dplyr)
library(gt)
library(gtsummary)
library(rio)
library(broom)
library(tidyr)
library(mice)

#Model for men and women combined
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
    #live_with = fct_explicit_na(live_with, "Missing"),
    liv_wth_child = cut(s1_q9, breaks = c(-Inf, 0, Inf),
                        labels = c("None", "One or more")),
    #liv_wth_child = fct_explicit_na(liv_wth_child, "Missing"),
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
    #depression = fct_explicit_na(depression, "Missing"),
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
    #tb_knowledge = fct_explicit_na(tb_knowledge, "Missing"),
    social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
    social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
    social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
    social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
    social_capital = fct_explicit_na(factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")), "Missing"),
    tb_rel_stigma = fct_explicit_na(factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")), "Missing"),
    tb_stigma_isolated = s5_q1_p + s5_q2_p + s5_q3_p + s5_q4_p,
    tb_stigma_disclosure = s1_q1_pd + s1_q2_pd + s1_q3_pd + s1_q4_pd + s1_q5_pd,
    hiv_stigma = hiv_stigma_sum,
    track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
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
model_data <- dplyr::select(data, class, gender_cat , calculated_age, race_cat,
                       edu_level , income , depression, anxiety, relat_status,
                       ever_had_tb ,  hiv_status , social_support_all_cat, social_capital,
                       hiv_stigma, tb_stigma, tb_knowledge,
                       medical_mistrust, liv_wth_child, tb_stigma_isolated,
                       tb_stigma_disclosure, live_alone_2weeks) %>%
  mutate(class_binary = case_when(
    class == 1 ~ 1,
    class != 1 ~ 0
  ))


imp <- mice(model_data)
fit_multinom_all <- with(imp, multinom(class ~  gender_cat + calculated_age  +
                            relat_status +  tb_knowledge + hiv_stigma +
                              + tb_stigma_isolated + tb_stigma_disclosure +
                   edu_level + income + depression + anxiety + liv_wth_child +
                   ever_had_tb +  hiv_status + social_support_all_cat +
                     medical_mistrust + live_alone_2weeks))

fit_log_all <- with(imp, glm(class_binary ~  gender_cat + calculated_age  +
                                    relat_status +  tb_knowledge + hiv_stigma +
                               + tb_stigma_isolated + tb_stigma_disclosure +
                                    edu_level + income + depression + anxiety + liv_wth_child +
                                    ever_had_tb +  hiv_status + social_support_all_cat +
                                    medical_mistrust + live_alone_2weeks,
                             family = "binomial"))

#make a table for the subgroups and the overall class
multinom_all_tab <- cbind(summary(pool(fit_multinom_all))[,1:2], round(summary(pool(fit_multinom_all), conf.int = TRUE, exponentiate = TRUE)[,3:9], 3)) %>%
  select(y.level, term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(3:6, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = "y.level", values_from = c(
    "estimate", "p.value", "conf_int")) %>%
  select(term, ends_with("_2"), ends_with("_3")) %>%
  mutate(estimate_2_ci = paste0(estimate_2, " (", conf_int_2, ")"),
         estimate_3_ci = paste0(estimate_3, " (", conf_int_3, ")")) %>%
  select(term, estimate_2_ci, p.value_2, estimate_3_ci, p.value_3)

log_all_tab <- cbind(summary(pool(fit_log_all))[,1], round(summary(pool(fit_log_all), conf.int = TRUE, exponentiate = TRUE)[-1], 3)) %>%
  select(term = 1, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(2:5, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  mutate(estimate_ci = paste0(estimate, " (", conf_int, ")")) %>%
  select(estimate_ci, p.value)

cbind(multinom_all_tab, log_all_tab) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "lightpink")),
            locations = cells_body(
              columns = p.value_2,
              rows = p.value_2 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "lightblue")),
            locations = cells_body(
              columns = p.value_3,
              rows = p.value_3 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "grey")),
            locations = cells_body(
              columns = p.value,
              rows = p.value < 0.05))

%>%
  gt::gtsave(filename = "Model overall.docx")


#Model for men only
data_men <- import(
  here("Model data men.csv")) %>%
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
    #live_with = fct_explicit_na(live_with, "Missing"),
    liv_wth_child = cut(s1_q9, breaks = c(-Inf, 0, Inf),
                        labels = c("None", "One or more")),
    #liv_wth_child = fct_explicit_na(liv_wth_child, "Missing"),
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
    #depression = fct_explicit_na(depression, "Missing"),
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
    #tb_knowledge = fct_explicit_na(tb_knowledge, "Missing"),
    social_support_all = rowSums(across(matches("^ss_.+_en$")), na.rm = FALSE)/12, #want it to be NA if one is missing
    social_support_sig_other = (ss_q1_en + ss_q2_en + ss_q5_en + ss_q10_en)/4,
    social_support_family = (ss_q3_en + ss_q4_en + ss_q8_en + ss_q11_en)/4,
    social_support_friends = (ss_q6_en + ss_q7_en + ss_q9_en + ss_q12_en)/4,
    social_capital = fct_explicit_na(factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")), "Missing"),
    tb_rel_stigma = fct_explicit_na(factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")), "Missing"),
    tb_stigma_isolated = s5_q1_p + s5_q2_p + s5_q3_p + s5_q4_p,
    tb_stigma_disclosure = s1_q1_pd + s1_q2_pd + s1_q3_pd + s1_q4_pd + s1_q5_pd,
    hiv_stigma = hiv_stigma_sum,
    track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
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

model_data_men <- dplyr::select(data_men, class, calculated_age, race_cat,
                                edu_level , income , depression, anxiety, relat_status,
                                ever_had_tb ,  hiv_status , social_support_all_cat, social_capital,
                                hiv_stigma, tb_stigma, tb_knowledge,
                                medical_mistrust, liv_wth_child, tb_stigma_isolated,
                                tb_stigma_disclosure, live_alone_2weeks) %>%
  mutate(class_binary = case_when(
    class == 1 ~ 1,
    class != 1 ~ 0
  ))

imp_men <- mice(model_data_men)

fit_multinom_men <- with(imp_men, multinom(class ~  calculated_age  +
                                         relat_status +  tb_knowledge + hiv_stigma +
                                         + tb_stigma_isolated + tb_stigma_disclosure +
                                         edu_level + income + depression + liv_wth_child +
                                         ever_had_tb +  hiv_status + social_support_all_cat +
                                         medical_mistrust + live_alone_2weeks))

fit_log_men <- with(imp_men, glm(class_binary ~  calculated_age  +
                               relat_status +  tb_knowledge + hiv_stigma +
                               + tb_stigma_isolated + tb_stigma_disclosure +
                               edu_level + income + depression + liv_wth_child +
                               ever_had_tb +  hiv_status + social_support_all_cat +
                               medical_mistrust + live_alone_2weeks,
                             family = "binomial"))




multinom_men_tab <- cbind(summary(pool(fit_multinom_men))[,1:2], round(summary(pool(fit_multinom_men), conf.int = TRUE, exponentiate = TRUE)[,3:9], 3)) %>%
  dplyr::select(y.level, term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(3:6, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  dplyr::select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = "y.level", values_from = c(
    "estimate", "p.value", "conf_int")) %>%
  dplyr::select(term, ends_with("_2"), ends_with("_3")) %>%
  mutate(estimate_2_ci = paste0(estimate_2, " (", conf_int_2, ")"),
         estimate_3_ci = paste0(estimate_3, " (", conf_int_3, ")")) %>%
  dplyr::select(term, estimate_2_ci, p.value_2, estimate_3_ci, p.value_3)


log_men_tab <- cbind(summary(pool(fit_log_men))[,1], round(summary(pool(fit_log_men), conf.int = TRUE, exponentiate = TRUE)[-1], 3)) %>%
  select(term = 1, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
  mutate(across(2:5, \(x) round(x, digits = 2)),
         conf_int = paste(conf.low, conf.high, sep = "-")) %>%
  mutate(estimate_ci = paste0(estimate, " (", conf_int, ")")) %>%
  select(estimate_ci, p.value)


cbind(multinom_men_tab, log_men_tab)%>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "lightpink")),
            locations = cells_body(
              columns = p.value_2,
              rows = p.value_2 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "lightblue")),
            locations = cells_body(
              columns = p.value_3,
              rows = p.value_3 < 0.05)) %>%
  tab_style(style = list(cell_fill(color = "grey")),
            locations = cells_body(
              columns = p.value,
              rows = p.value < 0.05)) %>%
  gt::gtsave(filename = "Model men.docx")








#Model for women only
data_women <- import("Model data women.csv") %>%
  mutate(# create

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
    depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
      "None-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)",
      "Moderate-Severe (15-19)", "Severe (20-27)")),
    #depression = fct_explicit_na(depression, "Missing"),
    medical_mistrust = cut(medical_misstrust, breaks = quantile(medical_misstrust, probs = c(0, 1/3, 2/3, 1), na.rm = T),
                           labels = c("Low", "Medium", "High")),
    #medical_mistrust = fct_explicit_na(medical_mistrust, "Missing"),
    anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
      "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
    #anxiety = fct_explicit_na(anxiety, "Missing"),
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
    track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
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

model_data_women <- dplyr::select(data_women, class, calculated_age  ,
                                  edu_level , income , depression, anxiety, relat_status,
                                  ever_had_tb ,  hiv_status , social_support_all_cat,
                                  medical_mistrust, liv_wth_child)

imp <- mice(model_data_women)
fit <- with(imp, glm(class ~  calculated_age  +
                       relat_status +
                       edu_level + income + depression + liv_wth_child +
                       ever_had_tb +  hiv_status + social_support_all_cat +
                       medical_mistrust ))

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
              rows = p.value < 0.05)) %>%
  gtsave(filename = "Model women.docx")

round(summary(pool(fit), conf.int = TRUE, exponentiate = TRUE), 3)
