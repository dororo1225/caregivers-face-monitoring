# load packages
library(tidyverse)
library(here)
library(ggpubr)
library(brms)
library(broom.mixed)
library(modelbased)
library(knitr)
library(bayestestR)

# read Data
here("Data", "data_facelooking.csv") %>%  
  read_csv(col_types = "cddcdddddddddddddddcccdddd") -> df
nrow(df)

# settings
prop_resize <- 0.5   # rescale rate
pixel_width <- 1920  # image with (pixel)
pixel_height <- 1080 # image height (pixel)

# Spatial distribution of face and eye-gaze in caregivers’ view
## Qualitative Analysis
df %>% 
  select(NameID, AgeinMonths, image_name, FaceLooking, Face_x, Face_y, Gaze_x, Gaze_y) %>% 
  pivot_longer(5:8, names_to = c("Measure", "coord"), names_sep = "_") %>% 
  pivot_wider(names_from = "coord", values_from = "value") %>% 
  mutate(FaceLooking = if_else(FaceLooking == 1, "Face look", "No face look")) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_density2d_filled() +
  geom_hline(yintercept = (prop_resize * pixel_height * 0.5), lty = 2, color = "white") +
  geom_vline(xintercept = (prop_resize * pixel_width * 0.5), lty = 2, color = "white") +
  guides(fill = "none") +
  facet_grid(Measure~FaceLooking) +
  xlim(0, prop_resize * pixel_width) +
  scale_y_reverse(limits = c(prop_resize * pixel_height, 0)) +
  # coord_fixed() +
  labs(x = "x", y = "y", tag = "a") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp1
print(gp1)

## Quantitative Analysis
df %>% 
  select(NameID, AgeinMonths, image_name, FaceLooking, Face_x, Face_y, Gaze_x, Gaze_y) %>% 
  pivot_longer(5:8, names_to = c("Measure", "coord"), names_sep = "_") %>%
  mutate(FaceLooking = if_else(FaceLooking == 1, "Face look", "No face look")) %>% 
  group_by(NameID, AgeinMonths, Measure, FaceLooking, coord) %>% 
  summarise(N = n(),
            Mean_coord = mean(value),
            .groups = "drop") -> df_coord

df_coord %>% 
  group_by(Measure, FaceLooking, coord) %>% 
  summarise(N = n(),
            Mean = mean(Mean_coord),
            SD = sd(Mean_coord),
            SE = SD/sqrt(N),
            .groups = "drop") %>%
  mutate(yint = if_else(coord == "x", pixel_width * prop_resize * prop_resize,
                        pixel_height * prop_resize * prop_resize))  %>% 
  ggplot(aes(x = FaceLooking, y = Mean, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 0.55) +
  geom_errorbar(aes(ymax = Mean + SD, ymin = Mean - SD), position = position_dodge(width = 0.9), width = 0.5) +
  geom_hline(aes(yintercept = yint), lty = 2, color = "grey25") +
  facet_grid(~coord) +
  labs(x = "Looking state", y = "Mean position (pixel)", fill = "AOI", tags = "b") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold")) -> gp2
print(gp2)

### Figure 5
ggarrange(gp1, gp2, ncol = 1, heights = c(4, 3)) %>% 
  ggexport(filename = here("Figures", "Figure5.jpg"), width = 1920 * 1.2, height = (1080/4)*7 *1.2, res = 300)

### x-coordinates
#### MCMC sampling
df_coord %>% 
  filter(coord == "x") %>% 
  mutate(visit = str_c(NameID, AgeinMonths, sep = "_")) %>% 
  brm(Mean_coord ~ Measure * FaceLooking + (1 + Measure * FaceLooking| visit), family = gaussian,
      data = .,
      iter = 21000,
      warmup = 1000,
      cores = 4,
      seed = 1234,
      # control = list(adapt_delta = 0.99, max_treedepth = 15),
      backend = "cmdstanr",
      refresh = 0) -> fit_x
summary(fit_x, robust = TRUE)

#### Parameter estimates (Supplementary Table S11)
tidy(fit_x, robust = TRUE)%>% kable()

#### Probability of direction
pd(fit_x, method = "direct", null = 0)

### y-coordinates
#### MCMC sampling
df_coord %>% 
  filter(coord == "y") %>%
  mutate(visit = str_c(NameID, AgeinMonths, sep = "_")) %>% 
  brm(Mean_coord ~ Measure * FaceLooking + (1 + Measure * FaceLooking |visit), family = gaussian,
      data = .,
      iter = 21000,
      warmup = 1000,
      cores = 4,
      seed = 1234,
      # control = list(adapt_delta = 0.99, max_treedepth = 15),
      backend = "cmdstanr",
      refresh = 0) -> fit_y
summary(fit_y, robust = TRUE)

#### Parameter estimates (Supplementary Table S12)
tidy(fit_y, robust = TRUE) %>% kable()

#### Probability of direction
pd(fit_y, method = "direct", null = 0)

#### Simple contrast analyses
estimate_contrasts(fit_y, contrast = "FaceLooking", at = "Measure")
estimate_contrasts(fit_y, contrast = "Measure", at = "FaceLooking")