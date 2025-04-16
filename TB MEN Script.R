library(rio)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lcmm)
library(cowplot)

#Load the data
data <- import(
  here("Aim 1 & 2 - TB Men Masterdataset (analysis) 19 April 2023.dta"))

#variables of interest
vars1 <-select(data, record_id, gender, calculated_age, s1_q1, track_comp,
                          matches("^ad_[1-9]|^ad_1[0-9]$")) %>%
  pivot_longer(cols = starts_with("ad_"), values_to = "adhere") %>%
  select(-name) %>%
  group_by(record_id) %>%
  mutate(vis_num = seq(1:n()))

vars2 <- select(data, record_id,
                          matches("^track_date_[1-9]|^track_date_1[0-9]$")) %>%
  pivot_longer(cols = starts_with("track_date"), values_to = "follow_up_date") %>%
  select(-name) %>%
  group_by(record_id) %>%
  mutate(vis_num = seq(1:n()))

vars3 <- select(data, record_id,
                matches("^track_date_pres_[1-9]|^track_date_pres_1[0-9]$")) %>%
  pivot_longer(cols = starts_with("track_date_pres_"), values_to = "presentation_date") %>%
  select(-name) %>%
  group_by(record_id) %>%
  mutate(vis_num = seq(1:n()))

#Final data set
combine_data <- left_join(vars1, vars2, by = c("record_id", "vis_num"), relationship = "many-to-many") %>%
  left_join(vars3, by = c("record_id", "vis_num")) %>%
  ungroup() %>%
  arrange(record_id, vis_num, follow_up_date, presentation_date)

#remove record id 57 since it has no observations
combine_data <- filter(combine_data, !(record_id %in%  c(57, 537, 810)))

combine_data <- group_by(combine_data, record_id) %>%
  mutate(days_missed = as.numeric( presentation_date - follow_up_date),
         days_missed = if_else(days_missed < 0, 0, days_missed), #need to come back to this line (crucial).
         cum_days = cumsum(days_missed), follow_up_date = as.Date(follow_up_date),
         presentation_date = as.Date(presentation_date))

combine_data <- group_by(combine_data, record_id) %>%
  mutate(months_since_start = (presentation_date - min(presentation_date, na.rm = TRUE)),
         months_since_start = round(as.numeric(months_since_start)/30.4375, 2)) %>%
  ungroup()

#Weird date values
combine_data <- mutate(combine_data, check = ifelse(lag(months_since_start) > months_since_start, "Yes", "No"))
combine_data <- mutate(combine_data, check2 = ifelse(lag(cum_days) > cum_days, "Yes", "No"))
combine_data <- mutate(combine_data, skip = ifelse(check == "Yes" | check2 == "Yes", 1, 0))
combine_data <- group_by(combine_data, record_id) %>% mutate(row_num = row_number())
combine_data <- mutate(combine_data, skip = ifelse(row_num == 1, 0, skip))



combine_data2 <- group_by(combine_data, record_id) %>%
  mutate(cum_days = ifelse(skip == 1, NA, cum_days),
         cum_days2 = ifelse(cumsum(is.na(cum_days)) > 0, NA, cum_days))


ggplot(combine_data2) +
  geom_line(aes(x = months_since_start, y = cum_days2, group = record_id))

#Linear model with only time as polynomial of degree 1
m1 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 1, idiag = TRUE, data = combine_data2,
           var.time = "months_since_start", link = 'linear')

m2 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 2, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data2,
           B = m1,link = 'linear')

m3 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 3, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data2,
           B = m1,link = 'linear')

#Linear model with polynomial of degree 2
m4 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 1, idiag = TRUE,
           var.time = "months_since_start",
           data = combine_data2, link = 'linear')

m5 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 2, idiag = TRUE,
           var.time = "months_since_start", data = combine_data2,
           B = m4,link = 'linear')

m6 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 3, idiag = TRUE,
           var.time = "months_since_start", data = combine_data2,
           B = m4,link = 'linear')

metrics <- data.frame(
  summarytable(m1, m2, m3, m4, m5,
               m6,
               which = c("G", "loglik", "entropy", "BIC", "%class"))) %>%
  arrange(BIC, loglik, entropy)

#Plot the trajectories
class_data <- m6$pprob[, c("record_id", "class")]
class_data$class <- as.factor(class_data$class)

final_data_lcmm <- left_join(combine_data2, class_data, by = "record_id")

#Using a loess smoother to assess patterns within each class
final_data_lcmm %>%
  filter(!is.na(class)) %>%
ggplot() +
  geom_smooth(aes(y = cum_days2, x= months_since_start, group = as.factor(class)), se = FALSE) +
  geom_line(aes(x = months_since_start, y = cum_days2, group = record_id), alpha= 0.1) +
  geom_vline(xintercept = 2) +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 180)) +
  labs(x = "Months since start of treatment", y = "Cumulative days missed") +
  facet_wrap(vars(class))


#Using actual predictions from the lcmm
prediction <- predictY(m6, newdata = data.frame(months_since_start = seq(0, 6, 0.1)),
                       draws = TRUE, ndraws = 2000)

plot(prediction)


ggplot()+
  geom_line(aes(x = months_since_start, y = cum_days2, group = record_id), alpha= 0.1,
            data = final_data_lcmm %>% filter(class == 2)) +
  geom_line(aes(x = months_since_start, y = Ypred_50_class2), data = plot_data_all) +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 100))

ggplot()+
  geom_line(aes(x = months_since_start, y = cum_days2, group = record_id), alpha= 0.1,
            data = final_data_lcmm %>% filter(class == 2)) +
  geom_smooth(aes(x = months_since_start, y = cum_days2), method = "loess",
              data = final_data_lcmm %>% filter(class == 2)) +
  geom_line(aes(x = months_since_start, y = Ypred_50_class2), data = plot_data_all) +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 100))

#ggplot graphs
plot_data_all <- cbind(data.frame(prediction[[1]]), data.frame(prediction[[2]]))
plot_data_all <- mutate(plot_data_all, group = "All")
plot_data_all <- mutate(plot_data_all, across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

labels_all <- round(metrics[1, c("X.class1", "X.class2", "X.class3")], 2)
legend_labels = c(paste0("Steady (", labels_all[1], "%)"),
                      paste0("Rapid increase after 2 months (", labels_all[2], "%)"),
                      paste0("Rapid increase from onset (", labels_all[3], "%)"))

(p_all <- ggplot(plot_data_all) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class1, col = legend_labels[1])) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class2, col = legend_labels[2])) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class3, col =  legend_labels[3])) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class1,
                    ymax = Ypred_97.5_class1, fill = legend_labels[1]), alpha = 0.1) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class2,
                    ymax = Ypred_97.5_class2, fill = legend_labels[2]), alpha = 0.1) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class3,
                    ymax = Ypred_97.5_class3, fill = legend_labels[3]), alpha = 0.1) +
    geom_vline(aes(xintercept = 2), linetype = "dashed") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 85)) +
    scale_color_manual(name = "",
                       values = setNames(c("red", "blue", "black"), legend_labels)) +
    scale_fill_manual(name = "",
                      values = setNames(c("red", "blue", "black"), legend_labels)) +
    labs(x = "Months since start of treatment", y = "Total days missed") +
    theme(legend.position = c(0.2, 0.9), legend.text=element_text(size=25),
          axis.title.x=element_text(size=25), axis.title.y = element_text(size=25),
          axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25)))


#Repeat models for men and women

#Men only
combine_data_men <- filter(combine_data2, gender == 1)

#Linear model with only time as polynomial of degree 1
m_men1 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 1, idiag = TRUE, data = combine_data_men,
           var.time = "months_since_start", link = 'linear')

m_men2 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 2, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data_men,
           B = m_men1,link = 'linear')

m_men3 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 3, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data_men,
           B = m_men1,link = 'linear')

#Linear model with polynomial of degree 2
m_men4 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 1, idiag = TRUE,
           var.time = "months_since_start",
           data = combine_data_men, link = 'linear')

m_men5 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 2, idiag = TRUE,
           var.time = "months_since_start", data = combine_data_men,
           B = m_men4,link = 'linear')

m_men6 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 3, idiag = TRUE,
           var.time = "months_since_start", data = combine_data_men,
           B = m_men4,link = 'linear')


metrics_men <- data.frame(
  summarytable(m_men1, m_men2, m_men3, m_men4, m_men5,
               m_men6,
               which = c("G", "loglik", "entropy", "BIC", "%class"))) %>%
  arrange(BIC, loglik, entropy)

class_data_men <- m_men6$pprob[, c("record_id", "class")]
class_data_men$class <- as.factor(class_data_men$class)

final_data_lcmm_men <- left_join(combine_data_men, class_data_men, by = "record_id")


final_data_lcmm_men %>%
  filter(!is.na(class)) %>%
  ggplot() +
  geom_smooth(aes(y = cum_days, x= months_since_start, group = as.factor(class)), se = FALSE) +
  geom_line(aes(x = months_since_start, y = cum_days, group = record_id), alpha= 0.1) +
  geom_vline(xintercept = 2) +
  scale_x_continuous(limits = c(0, 6)) +
  labs(x = "Months since start of treatment", y = "Cumulative days missed") +
  facet_wrap(vars(class))

prediction_men <- predictY(m_men6, newdata = data.frame(months_since_start = seq(0, 6, 0.1)),
                       draws = TRUE, ndraws = 2000)

#ggplot graphs
plot_data_men <- cbind(data.frame(prediction_men[[1]]), data.frame(prediction_men[[2]]))
plot_data_men <- mutate(plot_data_men, group = "Men")

plot_data_men <- mutate(plot_data_men, across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

labels_men <- round(metrics_men[1, c("X.class1", "X.class2", "X.class3")], 2)
legend_labels_men = c(paste0("Rapid increase after 2 months (", labels_men[1], "%)"),
                  paste0("Steady (", labels_men[2], "%)"),
                  paste0("Rapid increase from onset (", labels_men[3], "%)"))

(p_men <- ggplot(plot_data_men) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class1, col = legend_labels_men[1])) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class2, col = legend_labels_men[2])) +
    geom_line(aes(x = months_since_start, y = Ypred_50_class3, col =  legend_labels_men[3])) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class1,
                    ymax = Ypred_97.5_class1, fill = legend_labels_men[1]), alpha = 0.1) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class2,
                    ymax = Ypred_97.5_class2, fill = legend_labels_men[2]), alpha = 0.1) +
    geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class3,
                    ymax = Ypred_97.5_class3, fill = legend_labels_men[3]), alpha = 0.1) +
    geom_vline(aes(xintercept = 2), linetype = "dashed") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 85)) +
    scale_color_manual(
      name = "", values = setNames(c("blue", "red", "black"), legend_labels_men)) +
    scale_fill_manual(
      name = "", values = setNames(c("blue", "red", "black"), legend_labels_men)) +
    labs(x = "Months since start of treatment", y = "Total days missed") +
    theme(legend.position = c(0.2, 0.9), legend.text=element_text(size=25),
          axis.title.x=element_text(size=25), axis.title.y = element_text(size=25),
          axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25)))

#Women
combine_data_women <- filter(combine_data2, gender == 2)

#Linear model with only time as polynomial of degree 1
m_fem1 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 1, idiag = TRUE, data = combine_data_women,
           var.time = "months_since_start", link = 'linear')

m_fem2 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 2, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data_women,
           B = m_fem1,link = 'linear')

m_fem3 <- lcmm(cum_days2 ~ months_since_start, random = ~months_since_start,
           subject = 'record_id', ng = 3, mixture = ~months_since_start,
           idiag = TRUE, var.time = "months_since_start", data = combine_data_women,
           B = m_fem1,link = 'linear')

#Linear model with polynomial of degree 2
m_fem4 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 1, idiag = TRUE,
           var.time = "months_since_start",
           data = combine_data_women, link = 'linear')

m_fem5 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 2, idiag = TRUE,
           var.time = "months_since_start", data = combine_data_women,
           B = m_fem4,link = 'linear')

m_fem6 <- lcmm(cum_days2 ~ months_since_start + I(months_since_start^2),
           random = ~months_since_start + + I(months_since_start^2),
           ~months_since_start + I(months_since_start^2),
           subject = 'record_id', ng = 3, idiag = TRUE,
           var.time = "months_since_start", data = combine_data_women,
           B = m_fem4,link = 'linear')

metrics_women <- data.frame(
  summarytable(m_fem1, m_fem2, m_fem3, m_fem4, m_fem5,
               m_fem6,
               which = c("G", "loglik", "entropy", "BIC", "%class"))) %>%
  arrange(BIC, loglik, entropy)

class_data_women <- m_fem5$pprob[, c("record_id", "class")]
class_data_women$class <- as.factor(class_data_women$class)

final_data_lcmm_women <- left_join(combine_data_women, class_data_women, by = "record_id")

final_data_lcmm_women %>%
  group_by(record_id) %>%
  arrange(months_since_start) %>%
  ggplot() +
  geom_line(aes(x = months_since_start, y = cum_days, group = record_id))

final_data_lcmm_women %>%
  filter(!is.na(class)) %>%
  ggplot() +
  geom_smooth(aes(y = cum_days, x= months_since_start, group = as.factor(class)), se = FALSE) +
  geom_line(aes(x = months_since_start, y = cum_days, group = record_id), alpha= 0.1) +
  geom_vline(xintercept = 2) +
  scale_x_continuous(limits = c(0, 15)) +
  labs(x = "Months since start of treatment", y = "Cumulative days missed") +
  facet_wrap(vars(class))

prediction_women <- predictY(m_fem5, newdata = data.frame(months_since_start = seq(0, 6, 0.1)),
                           draws = TRUE, ndraws = 2000)

plot(prediction_women)
#ggplot graphs

plot_data_women <- cbind(data.frame(prediction_women[[1]]), data.frame(prediction_women[[2]]))
plot_data_women <- mutate(plot_data_women, group = "Women")
plot_data_women <- mutate(plot_data_women, across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

labels_women <- round(metrics_women[2, c("X.class1", "X.class2")], 2)
(legend_labels_women = c(paste0("Steady (", labels_women[2], "%)"),
                        paste0("Rapid increase from onset (", labels_women[1], "%)")))

(p_women <- ggplot(plot_data_women) +
  geom_line(aes(x = months_since_start, y = Ypred_50_class1, col = legend_labels_women[2])) +
  geom_line(aes(x = months_since_start, y = Ypred_50_class2, col = legend_labels_women[1])) +
  geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class1, ymax = Ypred_97.5_class1, fill = legend_labels_women[2]), alpha = 0.1) +
  geom_ribbon(aes(x = months_since_start, ymin = Ypred_2.5_class2, ymax = Ypred_97.5_class2, fill = legend_labels_women[1]), alpha = 0.1) +
    geom_vline(aes(xintercept = 2), linetype = "dashed") +
  theme_classic() +
    scale_y_continuous(limits = c(0, 85)) +
  scale_color_manual(name = "",
                     values = setNames(c("red", "black"), legend_labels_women)) +
  scale_fill_manual(name = "",
                    values = setNames(c("red", "black"), legend_labels_women)) +
  labs(x = "Months since start of treatment", y = "Total days missed") +
  theme(legend.position = c(0.2, 0.9), legend.text=element_text(size=25),
        axis.title.x=element_text(size=25), axis.title.y = element_text(size=25),
        axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25)))

(prow <- plot_grid(
  p_all ,
  p_men,
  p_women,
  align = 'vh',
  labels = c("a) Overall (N =657)", " b) Men (N = 446)", "c) Women (N = 211)"),
  label_size = 25,
  hjust = -1,
  nrow = 2
))

ggsave("Trajectories.png", prow, bg = "white", units = "px", width = 10500,
       height = 5000)

#Plot finishers versus class
lcmm_data <- group_by(final_data_lcmm, record_id) %>%
  slice(1) %>%
  ungroup()

track_comp_data <- filter(data,  !(record_id %in%  c(57, 537, 810))) %>%
  left_join(lcmm_data)

ggplot(track_comp_data) +
  geom_bar(aes(x = class, y = ..prop.., group = 1)) +
  facet_wrap(~factor(track_comp))
ge


default_data <-select(data, record_id, gender, calculated_age, s1_q1, track_comp,
                              matches("^def_[1-9]|^def_1[0-9]$")) %>%
  pivot_longer(cols = starts_with("def"), values_to = "default") %>%
  select(-name) %>%
  group_by(record_id) %>%
  mutate(vis_num = seq(1:n())) %>%
  mutate(default_true = case_when(
    any(default == 1) ~ "Defaulted",
    all(is.na(default)) ~ "Didn't default")) %>%
  ungroup() %>%
filter(!(record_id %in%  c(57, 537, 810))) %>%
  left_join(lcmm_data) %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup()

class_labels = c(`1` = "Steady",
                 `2` = "Rapid increase after 2 months",
                 `3` = "Rapid increase from onset")

(default_plot <- ggplot(default_data) +
  geom_bar(aes(x = factor(default_true),
               y = ..prop.. * 100, group = 1)) +
  scale_y_continuous(name = "Percent", breaks = seq(0,100, 20)) +
  facet_wrap(~class, labeller = as_labeller(class_labels)) +
  theme_classic() +
  theme(strip.text = element_text(size = 35), axis.title.x=element_blank(), axis.title.y = element_text(size=35),
        axis.text.x = element_text(size = 35), axis.text.y = element_text(size = 35)))


ggsave("Default.png", default_plot, bg = "white", units = "px", width = 10500,
       height = 5000)
prop.table(table(default_data$default_true, default_data$class))

gtsummary::tbl_cross(default_data, row = class, col = default_true,
                     percent = "row")

model_data <- filter(data, !(record_id %in%  c(57, 537, 810))) %>%
  left_join(lcmm_data, by = "record_id")
