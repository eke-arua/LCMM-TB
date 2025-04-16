library(ggplot2)
library(cowplot)
library(rio)
library(dplyr)
library(survival)
library(survminer)
library(ggpattern)
library(stringr)

data <- import("Model data.csv")

default_data <- select(data, record_id, class, matches("^def_."), track_comp.x) %>%
  mutate(across(matches("^def_."), \(x) ifelse(is.na(x), 0, x)),
    default = rowSums(across(matches("^def_.")), na.rm = FALSE),
    default_true = ifelse(default == 0, "Didn't default", "Defaulted"),
    track_complete = factor(
      track_comp.x, levels = 0:1,
      labels = c("Didn't complete", "Completed treatment")),
    class_cat = factor(class, levels = 1:3, labels = c(
      "Steady", "Rapid increase after 2 months", "Rapid increase from onset" )))

class_names <- c(
  `1` = "Steady",
  `2` = "Rapid increase after 2 months",
  `3` = "Rapid increase from onset"
)


(default_plot <- default_data %>%
  group_by(class_cat, default_true) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = class_cat, y = prop, fill = default_true),
                   stat = "identity", position = "dodge", size = 1, color = "black") +
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::percent, name = "Percent",
                     limits = c(0, 1)) +
  scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = 20)) +
  theme_classic() +
  theme( axis.text.x = element_text(size = 25), legend.title = element_blank(),
        axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25), legend.position = c(0.9,1),
        legend.justification = c(1,1), legend.direction = "vertical"))

complete_plot <- default_data %>%
  filter(!is.na(track_complete)) %>%
  group_by(class_cat, track_complete) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = class_cat, y = prop, fill = track_complete),
           stat = "identity", position = "dodge", size = 1, color = "black") +
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::percent, name = "Percent",
                     limits = c(0, 1)) +
  scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = 20)) +
  theme_classic() +
  theme( axis.text.x = element_text(size = 25), legend.title = element_blank(),
         axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 25),
         legend.text = element_text(size = 25), legend.position = c(0.9,1),
         legend.justification = c(1,1), legend.direction = "vertical",
         legend.spacing.x = unit(5, "cm"))


(final_plot <- plot_grid(
  default_plot, complete_plot, nrow = 1,
  align = 'vh',
  label_x = 0,
  label_y = 1,
  labels = c("a) Defaulting behaviour", " b) Treatment completion"),
  label_size = 35))

ggsave("Bar graphs.png", final_plot, units = "px", width = 11000,
       height = 6000)

vars2 <- select(data, record_id, matches("^def_."),
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

#create a default data
data_surv <- select(data, record_id, presentation_date,
                    follow_up_date, matches("^def_.")) %>%
  mutate(across(4:18, \(x) ifelse(is.na(x), 0, x)))

max_months <- group_by(combine_data2, record_id) %>%
  slice(n()) %>%
  ungroup() %>%
  group_by(gender) %>%
  summarise(
    med_time = median(months_since_start, na.rm = T),
    low_q = quantile(months_since_start, na.rm = T, probs = 0.25),
    high_q = quantile(months_since_start, na.rm = T, probs = 0.75))


lol <- plot_grid(A, B, C,
                 D, A, NULL, ncol=3, nrow=2,
                 align="hv", rel_widths = c(1, 1, 1, 1, 1, 1),
                 labels = c('A', 'B', 'C', 'D', 'E', ''))

