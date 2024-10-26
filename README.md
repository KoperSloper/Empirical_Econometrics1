# Replication Study on Collectivization and Draft Animal Inventories

This repository contains the materials for a replication study of Chen and Lan's (2017) analysis on the impact of collectivization on the inventory of draft animals in rural China. This study reproduces the results of the original paper and extends the analysis using recent econometric methods to address potential heterogeneity in treatment effects and evaluate the validity of the parallel trends assumption in a difference-in-differences (DiD) framework.

## Project Overview

**Original Study**  
Chen and Lan (2017) investigate how collectivization policies implemented by the Chinese Communist Party in the mid-20th century affected agricultural output, focusing on draft animals. By utilizing panel data from 1,720 counties in China, their analysis leverages the staggered timing of collectivization implementation across counties to estimate the policy’s effects on draft animal populations.

**Replication Results**  
The replication analysis uses a two-way fixed effects (TWFE) estimator and finds similar results to those in the original study, with effect estimates ranging between -3% to -4.9%. However, this replication identifies some concerns with the parallel trends assumption, which may affect the robustness of the findings. For the 1955 treatment group in the same year, estimates range between -6.32% and -6.35%.

**Extensions**  
Additional econometric methods are applied to investigate treatment effect heterogeneity and to assess the DiD framework's assumptions. This highlights the potential for methodological refinement in historical policy analysis.

## Repository Contents

- `Replication_study.r`: R script containing the code for reproducing the main analysis and estimates reported in this study. The code is structured to load data, run the TWFE estimations, and assess the parallel trends assumption. 

- `README.md`: This file.
