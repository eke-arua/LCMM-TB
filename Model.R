library(nnet)
library(dplyr)
library(gt)
library(gtsummary)
library(rio)
library(broom)
library(tidyr)
library(mice)

data <- import("Model data.csv") %>%
  mutate(
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
    social_support = rowSums(across(matches("^ss_.+_en$")), na.rm = TRUE),
    depression_score = rowSums(across(matches("^dep_.+_en$")), na.rm = TRUE),
    gender_norms = rowSums(across(matches("^gn_.+_en$")), na.rm = TRUE),
    alcohol_use = rowSums(across(matches("^aa_q.+_en$")), na.rm = TRUE),
    patient_satisfaction = rowSums(across(matches("^pc_q.+_en$")), na.rm = TRUE)
  )


# mutate(
#   gender_cat = factor(gender, levels = 1:2, labels = c("Male", "Female")),
#   relat_status = cut(s1_q3, breaks = c(-Inf, 1, 2, 3, Inf), labels = c(
#     "Single", "Relationship, unmarried", "Married", "Seperated/Divorced/Widowed")),
#   race_cat = factor(s1_q1, levels = c(1:4, 99), labels = c(
#     "Other", "Black", "Other", "Coloured", "Other" )),
#   born_SA = factor(s1_q2, levels = c(0:1), labels = c("No", "Yes")),
#   edu_level = cut(s1_q4, breaks = c(-Inf, 2, 4, Inf), labels = c(
#     "Primary and below", "Grade 8 -11 (before Matric)", "Matric and above")),
#   liv_condition = factor(s1_q5, levels = c(1:3, 99), labels = c(
#     "Informal dwelling", "Formal house", "Formal house", "Formal house")),
#   liv_condition = fct_explicit_na(liv_condition, "Missing"),
#   live_alone_2weeks = factor(s1_q6, levels = 1:2, labels = c("Yes", "No")),
#   live_with = factor(s1_q7, levels = 1:3, labels = c(
#     "Family or Partner", "Living with friends", " Living with strangers")),
#   live_with = fct_explicit_na(live_with, "Missing"),
#   num_adults = s1_q8,
#   num_child = s1_q9,
#   bread_winner = factor(s1_q11, levels = 1:2, labels = c("Yes", "No")),
#   income = cut(s1_q14, breaks = c(-Inf, 1, 2, Inf), labels = c(
#     "< 2000", "2000 - 5000", "> 5000")),
#   ever_had_tb = factor(s2_q1_had_tb, levels = c(1, 2, 3), labels = c(
#     "Never", "Yes, less than 2 years ago", "Yes, more than 2 years ago")),
#   know_any_tb = factor(s2_q2_currently_has_tb, levels = c(1, 2), labels = c("Yes", "No")),
#   hiv_status = cut(s2_q4_hiv_status, breaks = c(-Inf, 1, 2, Inf), labels = c(
#     "Positive", "Negative", "Unknown")),
#   depression = cut(phq9_sum, breaks = c(-Inf, 4, 9, 14, 19, Inf), labels = c(
#     "Non-Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderate-Severe (15-19)", "Severe (20-27)")),
#   depression = fct_explicit_na(depression, "Missing"),
#   anxiety = cut(gad7_sum, breaks = c(-Inf, 5, 10, 14, Inf), labels = c(
#     "Mild (0-5)", "Moderate (6-10)", "Moderate Sever (11-15)", "Moderate-Severe (15-21)")),
#   anxiety = fct_explicit_na(anxiety, "Missing"),
#   alcohol_use = fct_explicit_na(cut(aa_q1a_en, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes")), "Missing"),
#   tb_knowledge = factor(tb_know_cat, levels = 1:2, labels = c("Low", "High")),
#   tb_knowledge = fct_explicit_na(tb_knowledge, "Missing"),
#   social_capital = fct_explicit_na(factor(social_capital3, levels = 1:3, labels = c("Low (0-3)", "Med (4-6)", "High (7-9)")), "Missing"),
#   tb_rel_stigma = fct_explicit_na(factor(tb_stigma4, levels = 0:3, labels = c("0-Low", "1", "2", "3-High")), "Missing"),
#   track_comp = fct_explicit_na(factor(track_comp, levels = 0:1, labels = c("No", "Yes")), "Missing")
# )


#Model


male_data <- filter(data, gender_cat == "Male")

mod1 <- multinom(class ~ + gender_cat + calculated_age.x  +
                 edu_level2 + income + scale(depression_score) +
                   ever_had_tb +  hiv_status + social_support +
                   live_alone_2weeks  , data = male_data)

tidy(mod1, exponentiate = TRUE, conf.int = TRUE) %>%
  select(y.level, term, estimate, p.value, conf.low, conf.high) %>%
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


%>%
  gtsave("Model table.docx")


model_data <- dplyr::select(data, class, gender_cat , calculated_age.x  ,
                       edu_level2 , income , depression_score ,
                       ever_had_tb ,  hiv_status , social_support ,
                       live_alone_2weeks)

imp <- mice(model_data)
fit <- with(imp, multinom(class ~  gender_cat + calculated_age.x  +
                   edu_level2 + income + scale(depression_score) +
                   ever_had_tb +  hiv_status + social_support +
                   live_alone_2weeks))
(summary(pool(fit), conf.int = T))

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
