# NFL Travel Distance and Away Team Performance

This repository analyzes whether travel distance impacts away team performance in the National Football League (NFL) using game‑level data and advanced performance metrics.

## Overview

The purpose of this project is to investigate whether the physical distance NFL teams travel for away games helps explain the performance gap between home and away teams. Using play‑by‑play and schedule data from recent NFL seasons, this project compares away team outcomes—including win percentage, Expected Points Added (EPA), completion percentage, and total yards—to the distance traveled for each game. The analysis finds that while away teams generally perform worse than home teams, travel distance alone does not strongly explain this disadvantage.

### Interesting Insight

One of the key findings from this project is that longer travel distances do not consistently lead to worse away‑team performance. Visual analysis shows that metrics such as EPA, passing efficiency, and total yards display only weak relationships with travel distance, suggesting that home‑field advantage is driven more by environmental and situational factors than by distance traveled alone.

## Data Sources and Acknowledgements

The data used in this project come from the following sources:

NFLreadr package for NFL schedules and play‑by‑play data (2022–2024 seasons)  
Stadium latitude and longitude data compiled manually using publicly available information from Wikipedia  
Geosphere R package for calculating great‑circle distances between stadiums  

This project makes use of several open‑source R packages, including nflreadr, dplyr, tidyr, ggplot2, knitr, and geosphere.

## Current Plan

This project is currently in its early stages and is focused on exploring whether travel distance affects away team performance in the NFL. The initial plan is to gather relevant game‑level data, calculate travel distances between stadiums, and establish baseline performance differences between home and away teams. Early exploratory analysis will be used to identify potential patterns and guide more detailed investigation. Please visit our Plan Document for more details.

## Repo Structure

The repository is organized as follows:

NFL Distance Analysis.qmd – Main Quarto document containing the full analysis and visualizations
figures – Saved plots used in the analysis
README.md – Project overview and documentation
Code Appendix – Embedded within the Quarto document, containing all analysis code for reproducibility

## Authors

For any questions please email:
[Shaun Bittenbender](mailto:smb8620@psu.edu)
[Alenander Sabel](mailto:avs7570)]
