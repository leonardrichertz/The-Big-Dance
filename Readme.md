# The Big Dance: NCAA Women’s Basketball Three-Point Shooter Analysis

This project analyzes and predicts the best three-point shooters in the NCAA Women’s Basketball playoff tournament using advanced statistical modeling. Instead of relying solely on observed three-point percentages, we employ a hierarchical (mixed effects) binomial logistic regression model to estimate each player’s true shooting talent, accounting for player, team, and conference effects.

## Overview

- **Goal:** Identify the most skilled three-point shooters for the NCAA Women’s Basketball tournament by adjusting for sample size, team, and conference context.
- **Approach:** Use a hierarchical binomial model to estimate each player’s underlying shooting ability, providing a more reliable ranking than raw percentages.
- **Data Sources:** Player box scores and schedule data for the 2025-26 season, obtained via the `wehoop` R package.

## Features

- Downloads and processes player and schedule data for the current season.
- Merges player stats with team and conference information.
- Fits a hierarchical binomial logistic regression model to estimate player “true talent.”
- Produces ranked tables and visualizations of top shooters.
- Outputs CSV files and publication-ready figures for reporting.

## Key Files

- `the_big_dance.R`: Main analysis script.
- `top_10_shooters.csv`: Table of the top 10 shooters by estimated true talent.
- `all_player_estimates.csv`: Full dataset of player shooting estimates.
- `true_talent_vs_observed.png`: Visualization comparing observed and estimated shooting percentages.

## How It Works

1. **Data Acquisition:**  
   Loads player box scores and schedule data for the selected season.

2. **Data Preparation:**  
   Cleans and merges data, extracting relevant features for modeling.

3. **Modeling:**  
   Fits a hierarchical binomial logistic regression model with random effects for player, team, and conference.

4. **Results:**
   - Ranks players by estimated true shooting talent.
   - Visualizes the relationship between observed and estimated percentages.
   - Identifies players who are most over- or under-estimated by raw stats.

5. **Output:**  
   Saves tables and figures for use in reports or presentations.

## Requirements

- R (version 4.0 or higher recommended)
- R packages: `wehoop`, `dplyr`, `lme4`, `ggplot2`, `gt`

## Usage

1. Clone the repository.
2. Install required R packages.
3. Run `the_big_dance.R` to perform the analysis and generate outputs.
4. Use `report_template.Rmd` to create a formatted report.

## License

This project is for educational and research purposes.
