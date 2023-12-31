# load packages
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(lme4)
library(modelr)
library(performance)
library(see)
library(scales)
library(knitr)
library(ggpubr)
library(broom.mixed)
library(bayestestR)
options(mc.cores = parallel::detectCores())

cols <- c(viridis_pal()(8)[8:1], "black") # color setting for visualizaion

# read Data
here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") -> df
nrow(df)

# Developmental change in caregivers' face looking
## prepare data for stan
scale_vec <- function(x){return(as.vector(scale(x, center = TRUE, scale = TRUE)))} # scaleing function
df %>% 
  mutate(FaceDev_pitch = abs(Pitch),
         FaceDev_yaw = abs(Yaw),
         FaceDev_roll = abs(Roll)) %>% 
  select(FaceLooking, NameID, InfantPosture, MotherPosture, GazeDirection, AgeinMonths, Distance, starts_with("FaceDev"), Centering, Saliency_face) -> df_stan

df_stan %>% 
  mutate(across(AgeinMonths:Saliency_face, scale_vec)) %>% 
  mutate(ID_Pair = as.numeric(as.factor(NameID))) -> df_stan_z 

## check multicollinearity
fit <- glmer(FaceLooking ~ AgeinMonths + Distance + Centering + Saliency_face + FaceDev_pitch + FaceDev_yaw + FaceDev_roll + GazeDirection + InfantPosture + MotherPosture + (1 + AgeinMonths | NameID),
             data = df_stan_z,
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
             family = "binomial")
result_vif <- check_collinearity(fit) 
plot(result_vif)
result_vif

## model fitting
### MCMC sampling
fit <- brm(formula = FaceLooking ~ AgeinMonths + Distance + Centering + Saliency_face + FaceDev_pitch + FaceDev_yaw + FaceDev_roll + GazeDirection + InfantPosture + MotherPosture + (1 + AgeinMonths + Distance + Centering + Saliency_face + FaceDev_pitch + FaceDev_yaw + FaceDev_roll + GazeDirection + InfantPosture + MotherPosture | NameID),
           data = df_stan_z,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 21000,
           cores = 4,
           control = list(adapt_delta = 0.99),
           backend = "cmdstanr",
           seed = 1234)

summary(fit, robust = TRUE)

### Parameter estimates (Table 3)
tidy(fit, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit, method = "direct", null = 0)

## effect of age (group-level & individual-level: Supplementary Table S9)
fit %>% 
  gather_draws(`b_AgeinMonths`) %>% 
  median_hdi(.width = c(.95, .66)) %>% 
  mutate(NameID = "group-level") %>% 
  select(!.variable) -> df_global

fit %>%
  spread_draws(b_AgeinMonths, r_NameID[NameID, AgeinMonths]) %>%
  filter(AgeinMonths == "AgeinMonths") %>%
  median_hdi(.value = b_AgeinMonths + r_NameID, .width = c(.95, .66)) %>%
  ungroup() %>% 
  select(!AgeinMonths) %>% 
  mutate(NameID = str_c("individual-level ", NameID)) %>% 
  bind_rows(df_global) -> df_par

df_par %>% 
  filter(.width == 0.95) %>% 
  kable()

## predictied values
df_stan %>% 
  mutate(AgeinMonths_raw = AgeinMonths,
         AgeinMonths = scale(AgeinMonths, center = TRUE, scale = TRUE)[, 1]) %>% 
  select(starts_with("AgeinMonths")) %>% 
  distinct() -> d_age

df_stan %>%
  group_by(NameID) %>% 
  data_grid(AgeinMonths_raw = seq_range(AgeinMonths, by = 0.5)) %>%
  ungroup() %>% 
  left_join(d_age, by = "AgeinMonths_raw") %>% 
  mutate(Centering = 0,
         Distance = 0,
         FaceDev_pitch = 0,
         FaceDev_yaw = 0,
         FaceDev_roll = 0,
         Saliency_face = 0,
         MotherPosture = "Down",
         InfantPosture = "Sitting",
         GazeDirection = "Averted") %>% 
  add_epred_draws(fit, re_formula = ~ (1 + AgeinMonths + Distance + Centering + Saliency_face + FaceDev_pitch + FaceDev_yaw + FaceDev_roll + GazeDirection + InfantPosture + MotherPosture | NameID)) %>% 
  median_hdi() %>% 
  mutate(AgeinMonths = AgeinMonths_raw, .keep = "unused") -> df_predict

# Visualization
## Figure 3
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
print(gp1)

df_stan %>%
  filter(InfantPosture == "Sitting", MotherPosture == "Down", GazeDirection == "Averted") %>% 
  group_by(NameID, AgeinMonths) %>% 
  summarise(N_img = n(),
            PropFaceLooking = sum(FaceLooking)/N_img,
            .groups = "drop") %>% 
  ggplot(aes(x = AgeinMonths)) +
  geom_ribbon(data = df_predict, aes(ymax = .upper, ymin = .lower), alpha = 0.25) +
  geom_point(aes(y = PropFaceLooking, color = NameID, size = N_img)) +
  geom_line(data = df_predict, aes(y = .epred, color = NameID), linewidth = 1) +
  facet_wrap(~NameID, ncol = 4) +
  scale_x_continuous(breaks = 10:15) +
  scale_size_continuous(range = c(1, 4)) +
  scale_color_viridis_d() +
  labs(x = "Age in months", y = "Proportipn of face looking", color = "Pair", size = "# of frames", tag = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2
print(gp2)

ggarrange(gp1, gp2, widths = c(1, 2)) %>% 
  ggexport(filename = here("Figures", "Figure3.jpg"), width = 4000, height = 1500, res = 300)


## Figure 4a
fit %>% 
  gather_draws(`b_Intercept`, `b_AgeinMonths`, `b_Distance`, `b_Centering`, `b_Saliency_face`, `b_FaceDev_pitch`, `b_FaceDev_yaw`, `b_FaceDev_roll`, `b_GazeDirectionDirected`, `b_InfantPostureSitting`, `b_InfantPostureUpright`, `b_MotherPostureUpright`) %>% 
  median_hdi(.width = c(.95, .66)) -> df_fixed

df_fixed %>% 
  mutate(.variable = fct_recode(.variable,
                                Intercept = "b_Intercept",
                                `Age` = "b_AgeinMonths",
                                `Infant posture (upright)` = "b_InfantPostureUpright",
                                `Infant posture (sitting)` = "b_InfantPostureSitting",
                                `Caregiver posture (upright)` = "b_MotherPostureUpright",
                                `Face centering` = "b_Centering",
                                `Face distance` = "b_Distance",
                                `Face deviance (pitch)` = "b_FaceDev_pitch",
                                `Face deviance (yaw)` = "b_FaceDev_yaw",
                                `Face deviance (roll)` = "b_FaceDev_roll",
                                `Gaze direction (directed)` = "b_GazeDirectionDirected",
                                `Face saliency` = "b_Saliency_face")) %>%
  mutate(.variable = fct_rev(fct_relevel(.variable, "Intercept", "Age", "Face distance", "Face centering", "Face saliency",
                                         "Face deviance (pitch)", "Face deviance (yaw)", "Face deviance (roll)", "Gaze direction (directed)",
                                         "Infant posture (upright)", "Infant posture (sitting)", "Caregiver posture (upright)"))) %>% 
  ggplot(aes(y = .variable, x = .value, xmin = .lower, xmax = .upper)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_pointinterval(interval_size_domain = c(1, 5),
                     interval_size_range = c(0.6, 1.4),
                     fatten_point = 2) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  labs(x = "value", y = "Standardized parameters") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp
print(gp)

## Save data for Figure 4
write_csv(df_fixed, here("Results", "Result_facelooking_OD.csv"))