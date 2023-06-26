# load packages
library(tidyverse)
library(here)
library(rstan)
library(cmdstanr)
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

# Summary statistics of the proportion of hit images
df_view %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N_img = n(),
            Face_obs = sum(Hit),
            PropHit_obs = Face_obs/N_img,
            .groups = "drop") %>% 
  summarise(Mean = mean(PropHit_obs),
            SD = sd(PropHit_obs))

# Developmental change in presence of infants' faces in view
## prepare data for stan
df_view %>%
  group_by(NameID, AgeinMonths) %>% 
  summarise(N_img = n(),
            Face_obs = sum(Hit),
            .groups = "drop") %>% 
  mutate(Obs = str_c(NameID, AgeinMonths, sep = "_"),
         ID_obs = as.numeric(as.factor(Obs)),
         ID_pair = as.numeric(as.factor(NameID)),
         Age_z = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>% 
  relocate(Age_z, .after = "AgeinMonths") -> df_obs

df_recall %>% 
  left_join(select(df_view, image_name, Hit), by = "image_name") %>% 
  filter(Unknown != 1) %>% 
  group_by(NameID, AgeinMonths, Coder) %>% 
  summarise(N_img = n(),
            N_face = sum(Face),
            N_detect = sum(Face == 1 & Hit == 1),
            HitRate = N_detect/N_face,
            .groups = "drop") %>% 
  mutate(Obs = str_c(NameID, AgeinMonths, sep = "_"),
         ID_obs = as.numeric(as.factor(Obs))) -> df_recall_obs

df_obs %>%
  group_by(NameID) %>% 
  data_grid(AgeinMonths = seq_range(AgeinMonths, by = 0.5)) %>% 
  ungroup() %>% 
  left_join(distinct(select(df_obs, starts_with("Age"))), by = "AgeinMonths") %>% 
  mutate(ID_pair = as.numeric(as.factor(NameID)),
         Obs = str_c(NameID, AgeinMonths, sep = "_"),
         ID_obs_pred = as.numeric(as.factor(Obs))) -> df_predict

df_obs %>%
  data_grid(AgeinMonths = seq_range(AgeinMonths, by = 0.5)) %>% 
  left_join(distinct(select(df_obs, starts_with("Age"))), by = "AgeinMonths") -> df_predict_glob

data <- list(N_obs = nrow(df_obs),
             N_pair = length(unique(df_obs$NameID)),
             Y = df_obs$Face_obs,
             N_img = df_obs$N_img,
             X = df_obs$Age_z,
             ID_pair = df_obs$ID_pair,
             N_coding = nrow(df_recall_obs),
             q_detect_obs = df_recall_obs$HitRate,
             ID_obs = df_recall_obs$ID_obs,
             N_predict = nrow(df_predict),
             X_predict = df_predict$Age_z,
             ID_pair_pred = df_predict$ID_pair,
             N_predict_glob = nrow(df_predict_glob),
             X_predict_glob = df_predict_glob$Age_z)

## model fitting
## MCMC sampling with cmdstanr
here("model", "model.stan") %>% 
  cmdstan_model() -> model

## MCMC sampling with cmdstanr
fit <- model$sample(data = data,
                    chains = 4,
                    iter_warmup = 1000,
                    iter_sampling = 20000,
                    parallel_chains = 4,
                    max_treedepth = 15,
                    refresh = 0,
                    seed = 1234)

output_dir <- here("MCMCSamples")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit$save_output_files(dir = output_dir, basename = "model")

output_dir <- here("MCMCSamples")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit
print(fit, pars = c("beta0", "beta1", "tau_u", "rho_u", "sigma_q"), probs = c(0.025, 0.5, 0.975))

## Parameter estimates
tidy(fit, robust = TRUE, conf.method = "HPDinterval", conf.int = TRUE, ess = TRUE) %>% 
  filter(term %in% c("beta0", "beta1",  "tau_u[1]", "tau_u[2]", "rho_u[1,1]", "rho_u[1,2]", "rho_u[2,1]", "rho_u[2,2]", "sigma_q")) %>% 
  kable()

## Probability of direction
pd(fit, method = "direct", null = 0) %>% 
  filter(Parameter %in% c("beta0", "beta1",  "tau_u[1]", "tau_u[2]", "rho_u[1,1]", "rho_u[1,2]", "rho_u[2,1]", "rho_u[2,2]", "sigma_q"))

## effect of age (group-level & individual-level: Supplementary Table S7)
fit %>% 
  gather_draws(beta1) %>% 
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") -> df_global

fit %>%
  gather_draws(`beta1_local.*`, regex = TRUE) %>% 
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(ID_pair = as.numeric(str_sub(.variable, start = -1, end = -1)),
         NameID = str_c("individual-level ", LETTERS[ID_pair])) %>% 
  select(!ID_pair) %>% 
  relocate(NameID) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

## predicted values
### Estimated recall
fit %>% 
  gather_draws(`q_detect.*`, regex = TRUE) %>% 
  median_hdi() %>% 
  mutate(ID_obs = as.numeric(str_extract(.variable, pattern = "[[:digit:]]+"))) %>% 
  rename(Median_detect = .value) %>% 
  select(Median_detect, ID_obs) %>% 
  left_join(df_obs, by = "ID_obs") %>% 
  mutate(PropHit_obs = Face_obs/N_img) -> df_q_detect

### The "true" proportion of infants' faces in caregivers' view
fit %>%
  gather_draws(`q_face_pred.*`, regex = TRUE) %>% 
  median_hdi() %>% 
  filter(!str_detect(.variable, pattern = "_glob")) %>% 
  mutate(ID_obs_pred = as.numeric(str_extract(.variable, pattern = "[[:digit:]]+"))) %>% 
  left_join(df_predict, by = "ID_obs_pred") -> df_q_face

### Visualization (Figure)
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

df_q_face %>%
  ggplot(aes(x = AgeinMonths)) +
  geom_ribbon(aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_line(aes(y = .value, color = NameID), linewidth = 1) +
  geom_point(data = df_q_detect, aes(y = PropHit_obs, fill = Median_detect), shape = 21, size = 2) +
  facet_wrap(~NameID, ncol = 4) +
  scale_x_continuous(breaks = 10:15) +
  scale_color_viridis_d() +
  labs(x = "Age in months", y = "Proportion of images with infants' faces", fill = "Estimated\nrecall", color = "Pair", tag = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2

ggarrange(gp1, gp2, widths = c(1, 2)) %>% 
  ggexport(filename = here("Figures", "Figure2.jpg"), width = 4000, height = 1500, res = 300)

# Summary statistics of the estimated proportion of infants' faces in view
df_q_face %>% 
  summarise(Mean = mean(.value),
            SD = sd(.value)) %>% 
  kable()

