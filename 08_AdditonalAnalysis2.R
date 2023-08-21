# load packages
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(modelr)
library(scales)
library(ggpubr)
library(broom.mixed)
library(bayestestR)
library(knitr)

cols <- c(viridis_pal()(8)[8:1], "black") # color setting for visualizaion

# read Data
here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") -> df
nrow(df)

# Developmental change in caregivers' hand looking
## prepare data for stan
df %>% 
  filter(FaceLooking == 0) %>% 
  filter(HandinView == 1) %>% 
  nrow()

df %>% 
  filter(FaceLooking == 0) %>% 
  filter(HandinView == 1) %>% 
  group_by(NameID, AgeinMonths, HandLooking) %>%
  summarise(N = n(),
            .groups = "drop_last") %>% 
  mutate(Total = sum(N),
         Prop = N/Total) %>% 
  ungroup() %>% 
  filter(HandLooking == 1) %>% 
  mutate(AgeinMonths_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) -> df_stan

## model fitting
df_stan %>%
  brm(N | trials(Total) ~ AgeinMonths_z + (1 + AgeinMonths_z|NameID), family = binomial(),
      data = .,
      iter = 21000, warmup = 1000,
      backend = "cmdstanr", cores = 4, seed = 1225,
      # control = list(adapt_delta = 0.9999, max_treedepth = 15),
      refresh = 0) -> fit
summary(fit)

## Parameter estimates (Table 4)
tidy(fit, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

## Probability of direction
pd(fit, method = "direct", null = 0)

## effect of age (group-level & individual-level: Supplementary Table S13)
fit %>% 
  gather_draws(b_AgeinMonths_z) %>%
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(-.variable) -> df_global

fit %>% 
  spread_draws(b_AgeinMonths_z, r_NameID[NameID, AgeinMonths_z]) %>%
  filter(AgeinMonths_z == "AgeinMonths_z") %>%
  median_hdi(.value = b_AgeinMonths_z + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  select(-AgeinMonths_z) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

df_par %>% 
  mutate(NameID = fct_rev(NameID))%>%
  ggplot(aes(y = NameID, x = .value, xmin = .lower, xmax = .upper, color = NameID)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointinterval(interval_size_domain = c(1, 5),
                     interval_size_range = c(0.6, 1.4),
                     fatten_point = 2) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  labs(x = "value", y = "Effect of age in months", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp1
print(gp1)

## predictied values
df %>%
  group_by(NameID) %>% 
  data_grid(AgeinMonths = seq_range(AgeinMonths, by = 0.5),
            Total = 100) %>%
  left_join(distinct(select(df_stan, starts_with("AgeinMonths"))), by = "AgeinMonths") %>% 
  add_epred_draws(fit, re_formula = NULL) %>% 
  mutate(.epred = .epred/100) %>% 
  median_hdi() -> df_predict

df_stan %>% 
  rename(N_image = Total) %>% 
  ggplot(aes(x = AgeinMonths)) +
  geom_ribbon(data = df_predict, aes(y = .epred, ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_point(aes(y = Prop, color = NameID, size = N_image)) +
  geom_line(data = df_predict, aes(y = .epred, group = NameID, color = NameID), linewidth = 1) +
  facet_wrap(~NameID, ncol = 4) +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(1, 4)) +
  scale_x_continuous(breaks = 10:15) +
  labs(x = "Age in months", y = "Proportion of looking at\nHandled Objects or Hands", tag = "b",
       size = "# of frames", color = "Pair") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp2
print(gp2)

## Visualization (Figure 7)
ggarrange(gp1, gp2, widths = c(1, 2)) %>% 
  ggexport(filename = here("Figures", "Figure7.jpg"), width = 4000, height = 1500, res = 300)