# Community Ecology Data Analysis

This repository contains an R script for analyzing community ecology data. The script performs various data processing and analysis tasks, including data visualization, diversity analysis, and statistical modeling.

## File Structure

- `analysis.R`: Main R script for data analysis.

## Requirements

The following R packages are required to run the script:

- `tidyverse`
- `stringr`
- `dave`
- `vegan`
- `dendextend`
- `DarkDiv`
- `rnaturalearth`
- `tmap`
- `ggmap`
- `maps`
- `crs`
- `sp`
- `sf`
- `rgdal`
- `tree`

## Steps

### Step 1: Data Distribution on the Map

- Reads environmental data from `environmentaldata.csv`.
- Renames columns for correct coordinates.
- Reprojects data to WGS84.
- Downloads and plots the world map with data points.

### Step 2: Reading Species and Environmental Data

- Reads species data from `speciesdata.csv`.
- Checks data for errors and anomalies.
- Divides species into vascular plants and bryophytes.
- Reads and corrects environmental data.

### Step 3: Diversity Analysis

- Calculates species richness and Shannon diversity index.
- Analyzes dark diversity for bryophytes and vascular plants.

### Step 4: Shapiro Test and Correlation Test

- Performs Shapiro tests for normality on various data columns.
- Conducts correlation tests between dark diversity and environmental variables.

### Step 5: Models of Richness and Dark Diversity vs. Environmental Data

- Builds linear models to analyze the relationship between species richness, dark diversity, and environmental variables.
- Evaluates model residuals and significance.

### Step 6: Community Distance Matrix and Clustering

- Calculates distance matrices for vascular plants and bryophytes.
- Performs hierarchical clustering and determines the optimal number of clusters.

### Step 7: NMDS Analysis and RDA Analysis

- Conducts NMDS analysis for vascular plants and bryophytes.
- Analyzes diversity related to NMDS.
- Performs RDA analysis to see community composition and relation to environmental variables.

### Step 8: Environment Related to Ordination

- Fits environmental variables to NMDS ordination for vascular plants and bryophytes.

### Step 9: Species Distribution Modeling

- Models species distribution for selected vascular species using decision trees.
- Predicts species presence-absence based on environmental variables.

### Step 10: Correlations Between Species Richness and Dark Diversity

- Analyzes correlations between species richness and dark diversity.
- Conducts ANOVA analysis based on management type.

## Usage

1. Ensure all required packages are installed.
2. Set the working directory to the project folder.
3. Run the `analysis.R` script.

## License

This project is licensed under the MIT License.

## Authors

- Ali Hakimzadeh
- Piia Tomingas
- Blanca Luz Caleno Ruiz