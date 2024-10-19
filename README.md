# County-level Estimation of High Blood Pressure from Observational Health Kiosk Data

This repository contains all the code and data necessary to reproduce the analysis in our paper *"County-level Estimation of High Blood Pressure from Observational Health Kiosk Data"*. The analysis provides county-level estimates of hypertension prevalence across the United States, leveraging data from health kiosks from Pursuant LLC located in Walmarts and CVS's across the country, which offer a unique source of blood pressure readings.

## Problem Description

Traditional surveillance sources for hypertension, such as the National Health and Nutrition Examination Survey (NHANES) and the Behavioral Risk Factor Surveillance System (BRFSS), face significant limitations. NHANES, though comprehensive, suffers from limited sample size and high costs, reducing its power at the subnational level. BRFSS, while larger, relies solely on self-reported data and lacks the precision provided by biomarkers like blood pressure readings. 

Our observational health kiosk data addresses these gaps by providing direct blood pressure measurements across the U.S. population. This allows for more frequent, cost-effective, and precise monitoring of hypertension at a granular level. It has the potential to form the basis of a passive surveillance system for blood pressure, enabling timely public health interventions and policy decisions.

## Methodology

We employed a multilevel regression and poststratification (MRP) approach to estimate hypertension prevalence for all U.S. counties. This method combines individual-level health kiosk data with county-level population characteristics to produce reliable, granular estimates. By adjusting for demographic covariates such as age, sex, race, and ethnicity, the model accounts for the variation in blood pressure patterns across different population groups and geographic areas. 

## Repository Contents

- `data/`: Contains processed data files used for the analysis.
- `code/`: Contains all analysis and data acquisition code
- `R/`: Includes all custom functions and utilities needed for the analysis.
- `figures/`: Scripts to generate figures and visualizations presented in the paper.

## How to Reproduce the Analysis

1. Clone the repository.
2. Follow the instructions in `docs/setup.md` to install dependencies.
3. Run the main analysis scripts in `scripts/` to reproduce the results presented in the paper.

## Contact

For questions or issues, please open an issue or contact the authors.
