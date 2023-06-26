Caregivers' monitoring of infant faces at home
====

## Overview
This repository includes the R codes and dataset for the following paper:

Authors masked. (in review). Caregivers' monitoring of infant faces at home: developmental decrease in face looking, but not in face seeing.

## Description
This repository consists of 5 folders and 9 R scripts. Some R scripts output figures to the folder 'Figures', and output CSV files to the folder 'MCMCSamples' or 'Results'.

A simple folder structure is shown below:

- Data
  - data.csv
  - data_facelooking.csv
  - data_recall.csv
- Figures
  - Figure2.jpg
  - Figure3.jpg
  - Figure4.jpg
  - Figure5.jpg
  - Figure6.jpg
  - Figure7.jpg
  - Figure8.jpg
  - Figure9.jpg
  - Figure10.jpg
  - FigureS1.jpg
  - FigureS2.jpg
  - FigureS3.jpg
- MCMCSamples
- model
  - model.stan
- Results
  - Result_facelooking_OD.csv
  - Result_facelooking_RD.csv
- R scripts
  - 01_DetectorPerformance.R
  - 02_FaceInView.R
  - 03_DataProcessing.R
  - 04_overviewVariables.R
  - 05_FaceLooking.R
  - 06_GazeRandomization.R
  - 07_AdditionalAnalysis1.R
  - 08_AdditionalAnalysis2.R
  - 09_AdditionalAnalysis3.R
 
The R script `01_DetectorPerformance.R` was written to evaluate the precision or recall performance of the detector. The R scripts beginning with the numbers 02 through 06 are the codes for the primary analyses. The R scripts beginning with the numbers 07 through 09 are the codes for the additional analyses.

Although we placed numbers on the file names of the R scripts, these scripts can be executed in any order preferred. Note that the files `05_FaceLooking.R` and `06_GazeRandomization.R` need a long time to be processed. We placed CSV files `Result_facelooking_OD.csv` and `Result_facelooking_RD.csv` as the results of these R scripts in the folder ‘Results’.


## Data Structure
- data.csv & data_facelooking.csv
  - The `data.csv` is used with R scripts beginning with the numbers 01 through 04. The `data_facelooking.csv` is used with R scripts numbered 05 through 09. 

| Column Name      | Variable                  | Explanation                                                                                |
| ---------------- | ------------------------- | ------------------------------------------------------------------------------------------ |
| NameID           | qualitative               | Participant ID (A-H)                                                                       |
| AgeinMonths      | quantitative (discrete)   | Infants' age in months                                                                     |
| AgeinDays        | quantitative (integer)    | Infants' age in days                                                                       |
| image_name       | qualitative               | Name of the image file                                                                     |
| frame_id         | quantitative (integer)    | Frame number of the image                                                                  |
| Gaze_x           | quantitative (integer)    | Position of gaze Area of Interest (AOI) (x coordinates)                                     |
| Gaze_y           | quantitative (integer)    | Position of gaze AOI (y coordinates)                                                       |
| Hit              | quantitative (binary)     | Whether the image is hit or not (hit = 1; otherwise = 0)                                   |
| FalseAlarm       | quantitative (binary)     | Whether the image is a false alarm or not (false alarm = 1; otherwise = 0)                 |
| Face_x           | quantitative (continuous) | Position of face AOI (x coordinates)                                                       |
| Face_y           | quantitative (continuous) | Position of face AOI (y coordinates)                                                       |
| Face_r           | quantitative (continuous) | Radius of face AOI                                                                         |
| FaceLooking      | quantitative (binary)     | Caregiver's face looking state (face looking = 1; no face looking = 0; otherwise = NA)    |
| Distance         | quantitative (continuous) | Depth of an infant's face with respect to the camera (in meters)                           |
| Centering        | quantitative (continuous) | Distance of face AOI to the center of the field of view (in pixels)                        |
| Saliency         | quantitative (continuous) | Maximum visual saliency in face AOI ranging from 0 (low salience) to 1 (high salience)     |
| Pitch            | quantitative (continuous) | The up-and-down rotation of the head relative to the frontal plane (in degrees)           |
| Yaw              | quantitative (continuous) | The right-and-left rotation of the head relative to the frontal plane (in degrees)        |
| Roll             | quantitative (continuous) | The clockwise and counterclockwise rotation of the head relative to the frontal plane (in degrees) |
| GazeDirection    | qualitative               | Infants' gaze direction ('Directed' or 'Averted'; otherwise = NA)                         |
| InfantPosture    | qualitative               | Infants' posture ('Held', 'Reclined', 'Supine', 'Prone', 'Sitting', or 'Upright')         |
| MotherPosture    | qualitative               | Caregivers' posture ('Up' or 'Down')                                                      |
| Saliency_gaze    | quantitative (continuous) | Maximum visual saliency in gaze AOI ranging from 0 (low salience) to 1 (high salience)    |
| Saliency_image   | quantitative (continuous) | Maximum visual saliency in the whole image ranging from 0 (low salience) to 1 (high salience) |
| HandInView       | quantitative (binary)     | Whether infants' hands were in the caregivers' field of view (presence = 1; absence = 0)  |
| HandLooking      | quantitative (binary)     | Caregiver's hand looking state (hand looking = 1; no hand looking = 0; otherwise = NA)   |



- data_recall.csv
  - This csv file is used with 12_OtherAnalyses.R.

| Column Name     | Variable                | Explanation                                                                   |
| ----            | ----                    |   ----                                                                        |
| image_name      |qualitative              | Experiment (‘Exp1’, ‘Exp2’, ‘Exp3’, ‘Exp4’, or ‘Exp5’)                        |
| NameID          |qualitative              | Participant ID                                                                |
| AgeinMonths     |quantitative (continuous)| Infant's looking time to the first aggressive interaction in the movie phase  |
| Coder           |quantitative (continuous)| Infant's looking time to the second aggressive interaction in the movie phase |
| Unknown         |quantitative (continuous)| Infant's looking time to the third aggressive interaction in the movie phase  |
| Face            |quantitative (continuous)| Infant's looking time to the third aggressive interaction in the movie phase  |



## Software & Package Versions
- RStudio: 1.4.1717
- R: 4.0.3
- Stan: 2.26.1
- tidyverse: 1.3.1
- brms: 2.15.0
- rstan: 2.26.2
- cmdstanr: 0.3.0
- tidybayes: 3.0.0
- broom.mixed: 0.2.7
- BayesFactor: 0.9.12.4.3
- bayestestR: 0.11.5
- effectsize: 0.5
- broom.mixed: 0.2.7
- ggpubr: 0.4.0
- cowplot: 1.1.1
- ggrepl: 0.9.1
- here: 1.0.1


## The Author of this README File
- [Authour Masked](https://github.com/dororo1225)
