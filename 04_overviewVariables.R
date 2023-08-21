# load packages
library(tidyverse)
library(here)
library(knitr)
library(ggpubr)
library(lme4)
library(brms)
library(broom.mixed)
library(modelbased)
library(ggcorrplot)

# read Data
here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") -> df
nrow(df)

# Developmental changes in dyads' postures and infants' gaze direction  (Figure S4)
## Infant posture
df %>% 
  group_by(NameID, AgeinMonths, InfantPosture) %>% 
  summarise(N = n(),
            .groups = "drop") %>% 
  complete(NameID, AgeinMonths, InfantPosture) %>% 
  replace_na(list(N = 0)) %>%
  group_by(NameID, AgeinMonths) %>% 
  mutate(Total = sum(N)) %>% 
  ungroup() %>% 
  filter(Total != 0) %>% 
  mutate(Proportion = N/Total) -> df_infantposture

df_infantposture %>% 
  ggplot(aes(x = AgeinMonths, y = Proportion, fill = InfantPosture)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 10:15) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(begin = 3/5, end = 1) +
  facet_wrap(~NameID, ncol = 4) +
  labs(x = "Age in months", y = "Proportion of frames", color = "Name", fill = "Infant\nposture", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp1

## Caregiver posture
df %>% 
  group_by(NameID, AgeinMonths, MotherPosture) %>% 
  summarise(N = n(),
            .groups = "drop") %>%
  complete(NameID, AgeinMonths, MotherPosture) %>% 
  replace_na(list(N = 0)) %>%
  group_by(NameID, AgeinMonths) %>% 
  mutate(Total = sum(N)) %>% 
  ungroup() %>% 
  filter(Total != 0) %>% 
  mutate(Proportion = N/Total) -> df_motherposture

df_motherposture %>% 
  ggplot(aes(x = AgeinMonths, y = Proportion, fill = MotherPosture)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 10:15) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~NameID, ncol = 4) +
  labs(x = "Age in months", y = "Proportion of frames", color = "Name", fill = "Caregiver\nposture", tag = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp2

## Infant gaze direction
df %>% 
  group_by(NameID, AgeinMonths, GazeDirection) %>% 
  summarise(N = n(),
            .groups = "drop") %>%
  complete(NameID, AgeinMonths, GazeDirection) %>% 
  replace_na(list(N = 0)) %>%
  group_by(NameID, AgeinMonths) %>% 
  mutate(Total = sum(N)) %>%
  ungroup() %>%
  filter(Total != 0) %>% 
  mutate(Proportion = N/Total) -> df_gaze

df_gaze %>% 
  ggplot(aes(x = AgeinMonths, y = Proportion, fill = GazeDirection)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 10:15) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~NameID, ncol = 4) +
  labs(x = "Age in months", y = "Proportion of frames", color = "Name", fill = "Infant\ngaze\ndirection", tag = "c") +
  theme_bw() +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option = "B") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp3

ggarrange(gp1, gp2, gp3, ncol = 1) %>% 
  ggexport(filename = here("Figures", "FigureS4.jpg"), width = 4500, height = 3600, res = 300)

# Summary Statistics (Overall)
## continuos variables
df %>%
  mutate(FaceDev_pitch = abs(Pitch),
         FaceDev_yaw = abs(Yaw),
         FaceDev_roll = abs(Roll)) %>% 
  pivot_longer(c("Distance", "Centering", "FaceDev_pitch", "FaceDev_yaw", "FaceDev_roll", "Saliency_face"), names_to = "variable", values_to = "value") %>%  
  group_by(variable, NameID, AgeinMonths) %>% 
  summarise(Mean_visit = mean(value),
            .groups = "drop") %>% 
  mutate(variable = fct_rev(fct_relevel(variable,  "Distance", "Centering", "Saliency_face", "FaceDev_pitch", "FaceDev_yaw", "FaceDev_roll"))) -> df_continuous

df_continuous %>% 
  group_by(variable) %>% 
  summarise(Overall_Mean = mean(Mean_visit),
            Overall_SD = sd(Mean_visit)) %>% 
  arrange(desc(variable)) %>% 
  kable()

## categorical variables
### Infant posture
df_infantposture %>% 
  group_by(InfantPosture) %>% 
  summarise(Overall_Mean = mean(Proportion),
            Overall_SD = sd(Proportion)) %>% 
  kable()

### Caregiver posture
df_motherposture %>% 
  group_by(MotherPosture) %>% 
  summarise(Mean = mean(Proportion),
            SD = sd(Proportion)) %>% 
  kable()

### Infant gaze direction
df_gaze %>% 
  group_by(GazeDirection) %>% 
  summarise(Mean = mean(Proportion),
            SD = sd(Proportion)) %>% 
  kable()

# Summary Statistics(First/Last)
## Continuous variables
df_continuous %>% 
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  select(NameID, AgeinMonths, visit_num, variable, Mean_visit) %>%
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>% 
  ungroup() -> df_comparison

df_comparison %>% 
  group_by(variable, visit) %>% 
  summarise(Mean = mean(Mean_visit),
            SD = sd(Mean_visit),
            .groups = "drop") %>% 
  pivot_wider(names_from = "visit", values_from = c("Mean", "SD")) %>% 
  arrange(desc(variable)) %>% 
  kable()

## categorical variables
### Infant posture
df_infantposture %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>%
  group_by(InfantPosture, visit) %>% 
  summarise(Mean = mean(Proportion),
            SD = sd(Proportion)) %>% 
  pivot_wider(names_from = "visit", values_from = c("Mean", "SD")) %>% 
  kable()

### Caregiver posture
df_motherposture %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>%
  group_by(MotherPosture, visit) %>% 
  summarise(Mean = mean(Proportion),
            SD = sd(Proportion)) %>% 
  pivot_wider(names_from = "visit", values_from = c("Mean", "SD")) %>% 
  kable()

### Infant gaze direction
df_gaze %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>%
  group_by(GazeDirection, visit) %>% 
  summarise(Mean = mean(Proportion),
            SD = sd(Proportion)) %>% 
  pivot_wider(names_from = "visit", values_from = c("Mean", "SD")) %>% 
  kable()

# First-Last Comaprison
## Continuous variables
df_comparison %>%
  select(!c(visit_num, AgeinMonths)) %>% 
  pivot_wider(names_from = "visit", values_from = "Mean_visit") %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(t_val = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$statistic),
         df = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$parameter),
         raw_p = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$p.value)) %>% 
  unnest(c(t_val, df, raw_p)) %>% 
  select(!data) %>% 
  ungroup() %>% 
  arrange(desc(variable)) %>% 
  kable()

## Categorical variables
### Infant posture
df_infantposture %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>% 
  ungroup() %>% 
  select(NameID, InfantPosture, visit, Proportion) %>% 
  pivot_wider(names_from = "visit", values_from = "Proportion") %>% 
  group_by(InfantPosture) %>% 
  nest() %>% 
  mutate(t_val = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$statistic),
         df = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$parameter),
         raw_p = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$p.value)) %>% 
  unnest(c(t_val, df, raw_p)) %>% 
  select(!data) %>% 
  ungroup() %>% 
  mutate(p_adj = p.adjust(raw_p, method = "holm")) %>% 
  kable()

## Caregiver posture
df_motherposture %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>% 
  ungroup() %>% 
  select(NameID, MotherPosture, visit, Proportion) %>% 
  pivot_wider(names_from = "visit", values_from = "Proportion") %>% 
  group_by(MotherPosture) %>% 
  nest() %>% 
  mutate(t_val = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$statistic),
         df = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$parameter),
         raw_p = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$p.value)) %>% 
  unnest(c(t_val, df, raw_p)) %>% 
  select(!data) %>% 
  ungroup() %>% 
  kable()

## Infant gaze direction
df_gaze %>%
  group_by(NameID) %>% 
  mutate(visit_num = as.numeric(as.factor(as.character(AgeinMonths)))) %>% 
  filter(visit_num == first(visit_num) | visit_num == last(visit_num)) %>% 
  mutate(visit = if_else(visit_num == first(visit_num), "first", "last", NA_character_),
         .after = "visit_num") %>% 
  ungroup() %>% 
  select(NameID, GazeDirection, visit, Proportion) %>% 
  pivot_wider(names_from = "visit", values_from = "Proportion") %>% 
  group_by(GazeDirection) %>% 
  nest() %>% 
  mutate(t_val = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$statistic),
         df = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$parameter),
         raw_p = map(data, ~t.test(Pair(first, last) ~ 1, data =.)$p.value)) %>% 
  unnest(c(t_val, df, raw_p)) %>% 
  select(!data) %>% 
  ungroup() %>% 
  kable()

# Simple Correlation
df %>%
  mutate(FaceDev_pitch = abs(Pitch),
         FaceDev_yaw = abs(Yaw),
         FaceDev_roll = abs(Roll),
         AvertedGaze = if_else(GazeDirection == "Averted", 1, 0),
         InfantProne = if_else(InfantPosture == "Prone", 1, 0),
         InfantSitting = if_else(InfantPosture == "Sitting", 1, 0),
         InfantUpright = if_else(InfantPosture == "Upright", 1, 0),
         MotherDown = if_else(MotherPosture == "Down", 1, 0)) %>% 
  select(AgeinMonths, Distance, Centering, Saliency_face, starts_with("FaceDev"), AvertedGaze, InfantProne, InfantSitting, InfantUpright, MotherDown) %>%
  rename(`Age in months` = "AgeinMonths",
         `Face centering` = "Centering",
         `Face distance` = "Distance",
         `Face saliency` = "Saliency_face",
         `Face deviation pitch` = "FaceDev_pitch",
         `Face deviation yaw` = "FaceDev_yaw",
         `Face deviation roll` = "FaceDev_roll",
         `Gaze directtion [averted]` = "AvertedGaze",
         `Infant posture [prone]` = "InfantProne",
         `Infant posture [sitting]` = "InfantSitting",
         `Infant posture [upright]` = "InfantUpright",
         `Caregiver posture [down]` = "MotherDown") -> d_mat

d_mat %>% 
  cor(method = "spearman") %>% 
  round(1) %>% 
  ggcorrplot(colors = c("#6D9EC1", "white", "#E46726"),
             lab = TRUE, type = "lower",
             legend.title = "Correlation") +
  labs(tag = "a") +
  theme(plot.tag = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) -> gp1
print(gp1)

# compare maximum saliency between regions
df %>% 
  select(NameID, AgeinMonths, image_name, starts_with("Saliency")) %>%
  rename("Face" = Saliency_face,
         "Gaze" = Saliency_gaze,
         "Image" = Saliency_image) %>% 
  pivot_longer(c("Face", "Gaze", "Image")) -> df_saliency

df_saliency %>% 
  group_by(name) %>% 
  summarise(N = n(),
            Mean = mean(value),
            SD = sd(value),
            SE = SD/sqrt(N)) %>% 
  ggplot(aes(x = name, y = Mean, fill = name)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymax = Mean + SD, ymin = Mean - SD), width = 0.5) +
  labs(x = "Area of Interest", y = "Maximum saliency value", fill = "AOI", tag = "b") +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2
print(gp2)  

## model fitting
### MCMC sampling
df_saliency %>% 
  brm(value ~ name + (1 |image_name) + (1 |NameID),
      data = .,
      family = gaussian(),
      iter = 21000,
      warmup = 1000,
      cores = 4,
      seed = 1234,
      # control = list(adapt_delta = 0.9999, max_treedepth = 15),
      backend = "cmdstanr") -> fit
summary(fit, robust = TRUE)

### Parameter estimates (Supplementary Table S8)
tidy(fit, robust = TRUE, conf.method = "HPDinterval") %>% 
  kable()

### Probability of direction
pd(fit, method = "direct", null = 0)

### Multiple comparison
estimate_contrasts(fit, contrast = "name")

# Figure S5
ggarrange(gp1, gp2, widths = c(3, 2), align = "h") %>% 
  ggexport(filename = here("Figures", "FigureS5.jpg"), width = 3500, height = 2000, res = 300)
