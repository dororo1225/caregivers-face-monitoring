# load packages
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(modelr)
library(scales)
library(ggpubr)
library(knitr)
library(broom.mixed)
library(bayestestR)
options(mc.cores=parallel::detectCores())

cols <- c(viridis_pal()(8)[8:1], "black") # color setting for visualizaion

# read Data
here("Data", "data.csv") %>% 
  read_csv(col_types = "cddcdddddddddddddddcccdddd") %>% 
  select(image_name, NameID, AgeinMonths, Hit, FalseAlarm) -> df_view

here("Data", "data_recall.csv") %>%  
  read_csv(col_types = "ccdcdddd") -> df_recall

# Precision
df_view %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N = n(),
            Hit = sum(Hit == 1),
            FA = sum(FalseAlarm == 1),
            .groups = "drop") %>% 
  mutate(Total = Hit + FA, 
         Precision = Hit / Total) -> df_precision

## summary statistics
df_precision %>%
  summarise(Mean = mean(Precision),
            SD = sd(Precision),
            Max = max(Precision),
            Min = min(Precision)) %>% 
  kable()

## Precision across age
df_precision %>% 
  mutate(AgeinMonths_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>% # scaling
  brm(Hit | trials(Total) ~ AgeinMonths_z + (1 + AgeinMonths_z|NameID), family = binomial(), data = .,
      iter = 21000,
      warmup = 1000,
      cores = 4,
      seed = 1234,
      control = list(adapt_delta = 0.9999, max_treedepth = 15),
      backend = "cmdstanr",
      refresh = 0) -> fit
summary(fit, robust = TRUE)

### Parameter estimates (Supplementary Table S3)
tidy(fit, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit, method = "direct", null = 0)

### predicted values
df_precision %>% 
  mutate(AgeinMonths_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>%
  add_epred_draws(fit, re_formula = ~ (AgeinMonths_z|NameID)) %>% 
  median_hdi() %>% 
  mutate(.epred = .epred/Total,
         .lower = .lower/Total,
         .upper = .upper/Total) -> df_predict

### effect of age (group-level & individual-level: Supplementary Table S4)
fit %>% 
  gather_draws(`b_AgeinMonths_z`) %>% 
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(!.variable) -> df_global

fit %>%
  spread_draws(b_AgeinMonths_z, r_NameID[NameID, AgeinMonths_z]) %>%
  filter(AgeinMonths_z == "AgeinMonths_z") %>%
  median_hdi(.value = b_AgeinMonths_z + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  select(!AgeinMonths_z) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

### Visualization (Supplementary Figure S1)
df_par %>% 
  mutate(NameID = fct_rev(NameID))%>%
  ggplot(aes(y = NameID, x = .value, xmin = .lower, xmax = .upper, color = NameID)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_pointinterval(interval_size_domain = c(1, 5),
                     interval_size_range = c(0.6, 1.4),
                     fatten_point = 2) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  labs(x = "value", y = "Effect of age in months", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp1

df_predict %>% 
  ggplot(aes(x = AgeinMonths, y = Precision)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_point(aes(color = NameID)) +
  geom_line(aes(y = .epred, color = NameID), linewidth = 1) +
  facet_wrap(~NameID, ncol = 4) +
  scale_x_continuous(breaks = 10:15) +
  labs(x = "Age in months", y = "Precision", tag = "b", color = "Pair") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2

ggarrange(gp1, gp2, widths = c(1, 2)) %>% 
  ggexport(filename = here("Figures", "FigureS1.jpg"), width = 4000, height = 1500, res = 300)


# Recall
df_recall %>% 
  left_join(select(df_view, image_name, Hit), by = "image_name") %>% 
  filter(Unknown != 1) %>% 
  group_by(NameID, AgeinMonths, Coder) %>% 
  summarise(N_img = n(),
            N_face = sum(Face),
            N_detect = sum(Face == 1 & Hit == 1),
            HitRate = N_detect/N_face,
            .groups = "drop") -> df_recall_obs

# Summary statistics
df_recall_obs %>% 
  group_by(NameID, AgeinMonths) %>%
  summarise(Mean = mean(HitRate),
            .groups = "drop") %>%
  summarise(N = n(),
            GrandMean = mean(Mean),
            SD = sd(Mean),
            Max = max(Mean),
            Min = min(Mean)) %>% 
  kable()

## Recall across age
df_recall_obs %>% 
  mutate(AgeinMonths_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>% 
  brm(HitRate ~ AgeinMonths_z + (AgeinMonths_z|NameID) + (1 |Coder),
      data = .,
      family = gaussian(),
      warmup = 1000,
      iter = 21000,
      cores = 4,
      control = list(adapt_delta = 0.99),
      backend = "cmdstanr",
      refresh = 0,
      seed = 1234) -> fit
summary(fit, robust = TRUE)

### Parameter estimates (Supplementary Table S5)
tidy(fit, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit, method = "direct", null = 0)

### predicted values
df_recall_obs %>% 
  mutate(AgeinMonths_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>%
  add_epred_draws(fit, re_formula = ~ (AgeinMonths_z|NameID)) %>% 
  median_hdi() -> df_predict

### effect of age (group-level & individual-level: Supplementary Table S6)
fit %>%
  gather_draws(b_AgeinMonths_z) %>% 
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(!.variable) -> df_global

fit %>%
  spread_draws(b_AgeinMonths_z, r_NameID[NameID, AgeinMonths_z]) %>%
  filter(AgeinMonths_z == "AgeinMonths_z") %>%
  median_hdi(.value = b_AgeinMonths_z + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  select(!AgeinMonths_z) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

### Visualization (Supplementary Figure S2)
df_par %>% 
  mutate(NameID = fct_rev(NameID))%>%
  ggplot(aes(y = NameID, x = .value, xmin = .lower, xmax = .upper, color = NameID)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_pointinterval(interval_size_domain = c(1, 5),
                     interval_size_range = c(0.6, 1.4),
                     fatten_point = 2) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  labs(x = "value", y = "Effect of age in months", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp1

df_predict %>% 
  ggplot(aes(x = AgeinMonths)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_point(aes(y = HitRate, color = Coder), alpha = 0.5) +
  geom_line(aes(y = .epred), linewidth = 1) +
  facet_wrap(~NameID, ncol = 4) +
  scale_x_continuous(breaks = 10:15) +
  labs(x = "Age in months", y = "Recall", tag = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2

ggarrange(gp1, gp2, widths = c(1, 2)) %>%
  ggexport(filename = here("Figures", "FigureS2.jpg"), width = 4000, height = 1500, res = 300)

# F-1 Score
df_recall_obs %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(MeanRecall = mean(HitRate),
            .groups = "drop") %>% 
  left_join(df_precision, by = c("NameID", "AgeinMonths")) %>% 
  mutate(Fscore = 2 * MeanRecall * Precision / (MeanRecall + Precision)) -> df_F

# Summary statistics
df_F %>%
  summarise(Mean = mean(Fscore),
            SD = sd(Fscore),
            Max = max(Fscore),
            Min = min(Fscore)) %>% 
  kable()
