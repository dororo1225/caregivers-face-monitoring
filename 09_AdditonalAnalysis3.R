# load packages
library(tidyverse)
library(here)
library(knitr)
library(brms)
library(tidybayes)
library(modelr)
library(ggpubr)
library(scales)
library(broom.mixed)
library(bayestestR)
options(mc.cores=parallel::detectCores())

cols <- c(viridis_pal()(8)[8:1], "black") # color setting for visualizaion

# read Data
here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") -> df
nrow(df)

# Developmental change in infants' head orientations
## Pitch
### MCMC sampling
fit_pitch <- brm(Pitch ~ 1 + AgeinMonths + (1 + AgeinMonths | NameID), 
                 data = df,
                 family = gaussian(),
                 prior   = NULL, 
                 chains  = 4, 
                 cores = 4,
                 iter    = 21000,  
                 warmup  = 1000,
                 refresh = 0,
                 backend = "cmdstanr",
                 control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(fit_pitch, robust = TRUE)

### Parameter estimates (Table 5)
tidy(fit_pitch, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit_pitch, method = "direct", null = 0)

### effect of age (group-level & individual-level: Supplementary Table S14)
fit_pitch %>%
  gather_draws(b_AgeinMonths) %>%
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(-.variable) -> df_global

fit_pitch %>%
  spread_draws(b_AgeinMonths, r_NameID[NameID, AgeinMonths]) %>%
  filter(AgeinMonths == "AgeinMonths") %>%
  median_hdi(b_AgeinMonths_local = b_AgeinMonths + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  rename(.value = b_AgeinMonths_local) %>% 
  select(-AgeinMonths) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

### Figure 10a
df %>% 
  data_grid(AgeinMonths = seq_range(AgeinMonths, n = 12)) %>% 
  add_epred_draws(fit_pitch, re_formula = NA) %>% 
  median_hdi() %>% 
  ungroup() -> df_posterior_fit_pitch_global

df %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N = n(),
            Mean = mean(Pitch),
            SD = sd(Pitch),
            SE = SD/sqrt(N),
            upr = quantile(Pitch, probs = 0.975),
            lwr = quantile(Pitch, probs = 0.025),
            .groups = "drop") -> df_pitch_obs

df_posterior_fit_pitch_global %>% 
  ggplot(aes(x = AgeinMonths)) +
  # geom_point(data = df_pitch_obs, aes(y = Mean, color = NameID), size = 3) +
  geom_hline(aes(yintercept = 0), lty = 2, linewidth = 1) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_pointinterval(data = df_pitch_obs,
                     aes(y = Mean, ymax = Mean + SE, ymin = Mean - SE, color = NameID),
                     position = position_jitter(seed = 1234, height = 0, width = 0.1), size = 2, point_size = 3) +
  geom_line(aes(y = .epred), linewidth = 2) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(10, 15.5, by = 0.5)) +
  labs(x = "Age in months", y = "Mean pitch (degree)",
       color = "Pair") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp_pitch
print(gp_pitch)

## Yaw
### MCMC sampling
fit_yaw <- brm(Yaw ~ 1 + AgeinMonths + (1 + AgeinMonths | NameID), 
               data    = df,
               family = gaussian(),
               chains  = 4, 
               cores = 4,
               iter    = 21000, 
               warmup  = 1000,
               refresh = 0,
               backend = "cmdstanr",
               control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(fit_yaw, robust = TRUE)

### Parameter estimates (Table 5)
tidy(fit_yaw, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit_yaw, method = "direct", null = 0)

### effect of age (group-level & individual-level: Supplementary Table S15)
fit_yaw %>%
  gather_draws(b_AgeinMonths) %>%
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(-.variable) -> df_global

fit_yaw %>%
  spread_draws(b_AgeinMonths, r_NameID[NameID, AgeinMonths]) %>%
  filter(AgeinMonths == "AgeinMonths") %>%
  median_hdi(b_AgeinMonths_local = b_AgeinMonths + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  rename(.value = b_AgeinMonths_local) %>% 
  select(-AgeinMonths) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

### Figure 10b
df %>% 
  data_grid(AgeinMonths = seq_range(AgeinMonths, n = 12)) %>% 
  add_epred_draws(fit_yaw, re_formula = NA) %>% 
  median_hdi() %>% 
  ungroup() -> df_posterior_fit_yaw_global

df %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N = n(),
            Mean = mean(Yaw),
            SD = sd(Yaw),
            SE = SD/sqrt(N),
            upr = quantile(Yaw, probs = 0.975),
            lwr = quantile(Yaw, probs = 0.025),
            .groups = "drop") -> df_yaw_obs

df_posterior_fit_yaw_global %>% 
  ggplot(aes(x = AgeinMonths)) +
  # geom_point(data = df_yaw_obs, aes(y = Mean, color = NameID), size = 3) +
  geom_hline(aes(yintercept = 0), lty = 2, linewidth = 1) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_pointinterval(data = df_yaw_obs,
                     aes(y = Mean, ymax = Mean + SE, ymin = Mean - SE, color = NameID),
                     position = position_jitter(seed = 1234, height = 0, width = 0.1), size = 2, point_size = 3) +
  geom_line(aes(y = .epred), linewidth = 2) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(10, 15.5, by = 0.5)) +
  labs(x = "Age in months", y = "Mean yaw (degree)",
       color = "Pair") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp_yaw
print(gp_yaw)

## Roll
### MCMC sampling
fit_roll <- brm(Roll ~ 1 + AgeinMonths + (1 + AgeinMonths | NameID), 
                data    = df,
                family = gaussian(),
                chains  = 4, 
                cores = 4,
                iter    = 21000, 
                warmup  = 1000,
                refresh = 0,
                backend = "cmdstanr",
                control = list(adapt_delta = 0.999, max_treedepth = 15))
summary(fit_roll, robust = TRUE)

### Parameter estimates (Table 5)
tidy(fit_roll, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit_roll, method = "direct", null = 0)

### effect of age (group-level & individual-level: Supplementary Table S16)
fit_roll %>%
  gather_draws(b_AgeinMonths) %>%
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(-.variable) -> df_global

fit_roll %>%
  spread_draws(b_AgeinMonths, r_NameID[NameID, AgeinMonths]) %>%
  filter(AgeinMonths == "AgeinMonths") %>%
  median_hdi(b_AgeinMonths_local = b_AgeinMonths + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  rename(.value = b_AgeinMonths_local) %>% 
  select(-AgeinMonths) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

### Figure 10c
df %>% 
  data_grid(AgeinMonths = seq_range(AgeinMonths, n = 12)) %>% 
  add_epred_draws(fit_roll, re_formula = NA) %>% 
  median_hdi() %>% 
  ungroup() -> df_posterior_fit_roll_global

df %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N = n(),
            Mean = mean(Roll),
            SD = sd(Roll),
            SE = SD/sqrt(N),
            upr = quantile(Roll, probs = 0.975),
            lwr = quantile(Roll, probs = 0.025),
            .groups = "drop") -> df_roll_obs

df_posterior_fit_roll_global %>% 
  ggplot(aes(x = AgeinMonths)) +
  # geom_point(data = df_roll_obs, aes(y = Mean, color = NameID), size = 3) +
  geom_hline(aes(yintercept = 0), lty = 2, linewidth = 1) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_pointinterval(data = df_roll_obs,
                     aes(y = Mean, ymax = Mean + SE, ymin = Mean - SE, color = NameID),
                     position = position_jitter(seed = 1234, height = 0, width = 0.1), size = 2, point_size = 3) +
  geom_line(aes(y = .epred), linewidth = 2) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(10, 15.5, by = 0.5)) +
  labs(x = "Age in months", y = "Mean roll (degree)",
       color = "Pair") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp_roll
print(gp_roll)

# Figure10
gp_pitch +
  labs(tag = "a") -> g1

gp_yaw +
  labs(tag = "b") -> g2

gp_roll +
  labs(tag = "c") -> g3

ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "right") %>%
  ggexport(filename =  here("Figures", "Figure10.jpg"), width = 2000, height = 3000, res = 300)