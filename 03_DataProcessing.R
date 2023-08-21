# load packages
library(tidyverse)
library(here)
library(knitr)
library(ggpubr)
library(scales)

cols <- c(viridis_pal()(8)[8:1], "black") # color setting for visualizaion

# read Data
here("Data", "data.csv") %>% 
  read_csv(col_types = "cddcdddddddddddddddcccdddd") %>% 
  filter(Hit == 1) -> df_hit # hit images

# Descriptive Statistics
## Supplementary Figure S3
df_hit %>%
  select(NameID, AgeinMonths, InfantPosture, MotherPosture) %>%
  replace_na(list(InfantPosture = "Unknown")) %>%
  count(NameID, AgeinMonths, InfantPosture) %>%
  add_count(NameID, AgeinMonths, wt = n, name = "nn") %>%
  mutate(prop = n/nn,
         InfantPosture = fct_rev(fct_relevel(InfantPosture, "Sitting", "Upright", "Prone", "Supine", "Held", "Reclined", "Unknown"))) %>%
  ggplot(aes(x = AgeinMonths, y = prop, fill = InfantPosture)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~NameID, ncol = 4) +
  scale_fill_viridis_d() +
  theme_bw() +
  scale_x_continuous(breaks = 10:15) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Age in months", y =  "Proportion of frames", fill = "Infant\nposture", tag = "a") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp1

df_hit %>%
  select(NameID, AgeinMonths, InfantPosture, MotherPosture) %>%
  replace_na(list(MotherPosture = "Unknown")) %>% 
  count(NameID, AgeinMonths, MotherPosture) %>% 
  add_count(NameID, AgeinMonths, wt = n, name = "nn") %>%
  mutate(prop = n/nn) %>% 
  ggplot(aes(x = AgeinMonths, y = prop, fill = MotherPosture)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~NameID, ncol = 4) +
  theme_bw() +
  scale_x_continuous(breaks = 10:15) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Age in months", y =  "Proportion of frames", fill = "Caregiver\nposture", tag = "b") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) -> gp2

ggarrange(gp1, gp2, ncol = 1) %>% 
  ggexport(filename = here("Figures", "FigureS3.jpg"), width = 4000, height = 3000, res = 300)


# Data Exclusion
## exclude images without gaze coordinate data
df_hit %>% 
  filter(!is.na(Gaze_x)) -> df_hit1

## exclude images with outliers or abnormal values
rad_to_deg <- function(x){return(x * 180 / pi)}

df_hit1 %>%
  mutate(across(c("Pitch", "Yaw", "Roll"), rad_to_deg)) %>% # convert millimeter to meter
  mutate(Distance = Distance / 1000,                        # convert millimeter to meter
         Pitch = - Pitch,                                   # flip direction of Pitch coordinate
         Yaw = - Yaw                                        # flip direction of Yaw coordinate
  ) -> df_hit1_tmp

avg_dist <- mean(df_hit$Distance)
sd_dist <- sd(df_hit$Distance)
thr <- avg_dist + 4 * sd_dist # threshold for Distance

avg_roll <- mean(df_hit1_tmp$Roll)
sd_roll <- sd(df_hit1_tmp$Roll)
upr_roll <- avg_roll + 4 * sd_roll
lwr_roll <- avg_roll - 4 * sd_roll # threshold for Roll

df_hit1_tmp %>% 
  filter(Distance > 0, Distance < thr, Pitch < 90, Pitch > -90, Roll < upr_roll, Roll > lwr_roll) -> df_hit2

## exclude images with rare postures
df_hit2 %>% 
  mutate(InfantPosture = if_else(InfantPosture %in% c("Prone", "Sitting", "Upright"), InfantPosture, "Other"))  %>% 
  filter(InfantPosture != "Other") -> df_hit3

# Save data for the face looking analysis
df_hit3 %>% 
  write_csv("Data/data_facelooking.csv")
