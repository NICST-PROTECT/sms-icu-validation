# SMS-ICU validation
Validation of the customised SMS-ICU score for multi- continental use: a retrospective cohort study using critical care registry data

## Research question
How does the customised SMS-ICU score perform as a prognostic model for in-hospital mortality in adult intensive care patients across multiple continents?

## Hypothesis
We hypothesise that the customised SMS-ICU score will demonstrate acceptable discrimination, calibration and overall fit for in-hospital mortality amongst a multi-continental cohort of adult ICU patients.
- Acceptable discrimination will be defined as an area under the receiver operating
characteristic curve (AUROC) &gt;0.7.
- Acceptable calibration will be assessed by plotting calibration curves, deriving calibration-in-the-large, and calculating observed/expected ratios (standardised mortality ratios [SMRs]).
- Acceptable overall fit will be assessed by calculating Brier scores.

## File descriptions
resources/SOP for SMS-ICU score validation.pdf -> Contains all instructions and steps to be followed for data mapping and code execution  
resources/SMS ICU_Data dictionary for mapping.xlsx -> Data dictionary to be used for data mapping  
resources/functions_needed.R -> Contains all required functions that must be loaded before executing the other code files

The following files should be executed in sequence. Each file depends on the successful completion of the previous one.
1. 0_data_mapping_validation.R -> Code for validating the data mapping process
2. 1_hospital_mortality_validation.R -> Code for applying inclusion/exclusion criteria, imputing missing data, calculating scores, and validating hospital mortality
3. 2_icu_mortality_validation.R -> Code for applying inclusion/exclusion criteria, imputing missing data, calculating scores, and validating ICU mortality

## Dependencies
ggplot2  
pROC  
CalibrationCurves  
DescTools  
glue  
dplyr  
readr  
ems
