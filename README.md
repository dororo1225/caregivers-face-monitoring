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
  - This csv file is used with 01_DetectorPerformance.R.

| Column Name   | Variable                  | Explanation                                                                       |
| ------------- | ------------------------- | --------------------------------------------------------------------------------- |
| image_name    | qualitative               | Name of the image                                                                 |
| NameID        | qualitative               | Participant ID (A-H)                                                              |
| AgeinMonths   | quantitative (discrete)   | Infants' age in months                                                            |
| Coder         | qualitative               | Coder ID (1-5)                                                                    |
| Unknown       | quantitative (binary)     | Whether the coder could not make a decision about the face in the view (could not make a decision = 1; otherwise = 0) |
| Face          | quantitative (binary)     | Whether infants' faces were in the caregivers' field of view (presence = 1; absence = 0)   |


## Software & Package Versions
- RStudio: 2023.06.0+421
- R: 4.2.1 
- Stan: 2.31.0
- tidyverse: 1.3.2
- brms: 2.17.0
- rstan: 2.26.13
- cmdstanr: 0.5.3
- tidybayes: 3.0.2
- lme4: 1.1.29
- broom.mixed: 0.2.9.4
- scales: 1.2.0
- modelr: 0.1.8
- performance: 0.10.2 
- bayestestR: 0.13.0
- modelbased: 0.8.6
- see : 0.7.2
- ggpubr: 0.4.0
- ggcorrplot: 0.1.4
- here: 1.0.1
- knitr: 1.39

## The Author of this README File
- [Authour Masked](https://github.com/dororo1225)
