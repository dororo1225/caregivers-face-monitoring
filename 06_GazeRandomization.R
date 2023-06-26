# read packages
library(tidyverse)
library(here)
library(brms)
library(tidybayes)
options(mc.cores = parallel::detectCores())

# parameter settings
prop_resize <- 0.5
pixel_width <- 1920
pixel_height <- 1080
fov <- 90
radius_va <- 4
radius_pixel <- sqrt(pixel_height^2 + pixel_width^2) * 0.5 * tan(radius_va * pi /180) / tan(fov*pi/(2*180)) * prop_resize

# scaling function
scale_vec <- function(x){return(as.vector(scale(x, center = TRUE, scale = TRUE)))}

here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") %>% 
  arrange(NameID, AgeinMonths) -> df
nrow(df)

df %>% 
  mutate(FaceDev_pitch = abs(Pitch),
         FaceDev_yaw = abs(Yaw),
         FaceDev_roll = abs(Roll)) %>% 
  select(FaceLooking, NameID, InfantPosture, MotherPosture, GazeDirection, AgeinMonths, Distance, starts_with("FaceDev"), Centering, Saliency_face) %>%
  mutate(across(AgeinMonths:Saliency_face, scale_vec)) %>% 
  mutate(ID_Pair = as.numeric(as.factor(NameID))) -> df_stan

# model fitting with the original data
fit <- brm(formula = FaceLooking ~ AgeinMonths + Distance + Centering + Saliency_face + FaceDev_pitch + FaceDev_yaw + FaceDev_roll + GazeDirection + InfantPosture + MotherPosture + (1 + AgeinMonths | NameID),
           data = df_stan,
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 3500,
           chains = 4,
           cores = 4,
           threads = threading(9),
           backend = "cmdstanr",
           seed = 1234)

summary(fit, robust = TRUE)
fit %>% 
  gather_draws(`b_Intercept`, `b_AgeinMonths`, `b_Distance`, `b_Centering`, `b_Saliency_face`, `b_FaceDev_pitch`, `b_FaceDev_yaw`, `b_FaceDev_roll`, `b_GazeDirectionDirected`, `b_InfantPostureSitting`, `b_InfantPostureUpright`, `b_MotherPostureUpright`) %>% 
  median_hdi() -> df_OD

# eye-gaze randomization
set.seed(1234)
tibble(.variable = character(),
       .value = numeric(),
       .lower = numeric(),
       .upper = numeric(),
       .width = numeric(),
       .point = character(),
       .interval = character(),
       Rhat_max = numeric(),
       RepNum = numeric()) -> df_RD

RepNum <- 1000

for (i in 1:RepNum){
  print(i)
  
  df %>%
    select(NameID, AgeinMonths, starts_with("Gaze_")) %>% 
    sample_n(nrow(.), replace = FALSE) %>%
    group_by(NameID, AgeinMonths) %>%
    mutate(rnd_id = row_number()) %>%
    ungroup() %>% 
    arrange(NameID, AgeinMonths, rnd_id) %>% 
    rename(rnd_Gaze_x = "Gaze_x",
           rnd_Gaze_y = "Gaze_y") %>% 
    select(contains("rnd")) -> df_random
  
  df %>% 
    bind_cols(df_random) %>% 
    mutate(FaceLooking = if_else((rnd_Gaze_x - Face_x)^2 + (rnd_Gaze_y - Face_y)^2 - (Face_r + radius_pixel)^2 < 0, 1, 0)) %>% 
    mutate(FaceDev_pitch = abs(Pitch),
           FaceDev_yaw = abs(Yaw),
           FaceDev_roll = abs(Roll)) %>% 
    select(FaceLooking, NameID, InfantPosture, MotherPosture, GazeDirection, AgeinMonths, Distance, starts_with("FaceDev"), Centering, Saliency_face) %>%
    mutate(across(AgeinMonths:Saliency_face, scale_vec)) %>% 
    mutate(ID_Pair = as.numeric(as.factor(NameID))) -> df_stan
  
  fit_tmp <- update(fit, newdata = df_stan,
                    chains = 4,
                    cores = 4,
                    threads = threading(9))
  r_hat <- max(rhat(fit_tmp), na.rm = TRUE)
  fit_tmp %>% 
    gather_draws(`b_Intercept`, `b_AgeinMonths`, `b_Distance`, `b_Centering`, `b_Saliency_face`, `b_FaceDev_pitch`, `b_FaceDev_yaw`, `b_FaceDev_roll`, `b_GazeDirectionDirected`, `b_InfantPostureSitting`, `b_InfantPostureUpright`, `b_MotherPostureUpright`) %>% 
    median_hdi() %>% 
    mutate(Rhat_max = r_hat,
           RepNum = i) -> df_tmp
  
  bind_rows(df_RD, df_tmp) -> df_RD
}

df_RD %>%
  write_csv(here("Rsults", "Result_facelooking_RD.csv"))

# compare orinal data and randomized data
## Original Data
here("Results", "Result_facelooking_OD.csv") %>%
  read_csv(col_types = "cddddcc") %>% 
  mutate(.variable = fct_recode(.variable,
                                Intercept = "b_Intercept",
                                `Age` = "b_AgeinMonths",
                                `Infant posture [upright]` = "b_InfantPostureUpright",
                                `Infant posture [sitting]` = "b_InfantPostureSitting",
                                `Caregiver posture [upright]` = "b_MotherPostureUpright",
                                `Face centering` = "b_Centering",
                                `Face distance` = "b_Distance",
                                `Face deviance pitch` = "b_FaceDev_pitch",
                                `Face deviance yaw` = "b_FaceDev_yaw",
                                `Face deviance roll` = "b_FaceDev_roll",
                                `Gaze direction [directed]` = "b_GazeDirectionDirected",
                                `Face saliency` = "b_Saliency_face")) %>%
  mutate(.variable = fct_rev(fct_relevel(.variable, "Intercept", "Age", "Face distance", "Face centering", "Face saliency",
                                         "Face deviance pitch", "Face deviance yaw", "Face deviance roll", "Gaze direction [directed]",
                                         "Infant posture [upright]", "Infant posture [sitting]", "Caregiver posture [upright]"))) -> df_OD

here("Results", "Result_facelooking_RD.csv") %>% 
  read_csv( col_types = "cddddccdd") %>% 
  mutate(.variable = fct_recode(.variable,
                                Intercept = "b_Intercept",
                                `Age` = "b_AgeinMonths",
                                `Infant posture [upright]` = "b_InfantPostureUpright",
                                `Infant posture [sitting]` = "b_InfantPostureSitting",
                                `Caregiver posture [upright]` = "b_MotherPostureUpright",
                                `Face centering` = "b_Centering",
                                `Face distance` = "b_Distance",
                                `Face deviance pitch` = "b_FaceDev_pitch",
                                `Face deviance yaw` = "b_FaceDev_yaw",
                                `Face deviance roll` = "b_FaceDev_roll",
                                `Gaze direction [directed]` = "b_GazeDirectionDirected",
                                `Face saliency` = "b_Saliency_face")) %>%
  mutate(.variable = fct_rev(fct_relevel(.variable, "Intercept", "Age", "Face distance", "Face centering", "Face saliency",
                                         "Face deviance pitch", "Face deviance yaw", "Face deviance roll", "Gaze direction [directed]",
                                         "Infant posture [upright]", "Infant posture [sitting]", "Caregiver posture [upright]"))) -> df_RD
  
df_OD %>% 
  ggplot(aes(y = .variable, x = .value, xmin = .lower, xmax = .upper)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_pointinterval(interval_size_domain = c(1, 5),
                     interval_size_range = c(0.6, 1.4),
                     fatten_point = 2) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  labs(x = "Value", y = "Standardized parameter", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp1
print(gp1)

expand.grid(Data = c("Original data", "Randomized data"),
            .variable = df_OD$.variable) %>% 
  as_tibble() %>% 
  mutate(.value = NA_real_) -> df_legend

df_RD %>% 
  filter(Rhat_max < 1.1) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_halfeye(color = "#E16462FF", fill = "#E16462FF", slab_alpha = 0.5) +  
  geom_point(data = df_OD, aes(x = .value, y = .variable), size = 3) +
  geom_point(data = df_legend, aes(color = Data)) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "#E16462FF")) +
  labs(x = "Value", y = "Standardized parameter", color = "Posterior\nmedian", tag = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2
print(gp2)

# Figure 6
ggarrange(gp1, gp2, widths = c(3, 4)) %>% 
  ggexport(filename = here("Figures", "Figure6.jpg"), width = 4000, height = 1500, res = 300)

# Supplementary Table S10
df_RD %>% 
  filter(Rhat_max < 1.1) %>% 
  group_by(.variable) %>% 
  summarise(Mean = mean(.value),
            SD = sd(.value),
            upr = quantile(.value, probs = 0.975),
            lwr = quantile(.value, probs = 0.025)) %>% 
  left_join(select(filter(df_OD, .width == "0.95"), .variable, .value), by = ".variable") %>% 
  select(.variable, .value, Mean, SD, lwr, upr) %>% 
  arrange(desc(.variable)) %>% 
  kable()