library(pROC)
library(ggplot2)
library(DescTools)
library(glue)
library(dplyr)
library(readr)
library(ems)
library(rms)

validate_columns <- function(data, columns_of_interest) {
  
  numeric_cols <- c('age', 'systolic_bp')
  alive_dead_cols <- c('hos_dis_status', 'icu_dis_status')
  adm_type_cols <- c('adm_type')
  errors <- c()
  
  numeric_validation <- sapply(numeric_cols, function(col) {
    if (!is.numeric(data[[col]])) {
      errors <<- c(errors, paste("Column", col, 
                                 "contains non-numeric and non-NA values"))
    } 
  })
  
  validate_col <- function(cols, valid_values, error_message) {
    for (col in cols) {
      if (!all(is.na(data[[col]]) | data[[col]] %in% valid_values)) {
        errors <<- c(errors, paste("Column", col, error_message))
      }
    }
  }


  validate_col(alive_dead_cols, valid_values = c("Alive", "Dead"),
               error_message = "contains values other than 'Alive', 'Dead', or NA")
  validate_col(adm_type_cols, valid_values = c("Medical", "Elective", "Emergency"),
               error_message = "contains values other than 'Medical', 'Elective', 'Emergency' or NA")
  
  yes_no_cols <- setdiff(columns_of_interest, c(numeric_cols, alive_dead_cols, adm_type_cols))
  validate_col(yes_no_cols, valid_values = c("Yes", "No"), 
               error_message = "contains values other than 'Yes', 'No', or NA")
  
  
  if (length(errors) > 0) {
    cat(paste(errors, collapse = "\n"))
  } else {
    cat("All validations passed!")
  }
  
}

inclusion_exclusion_criteria <- function(data, type, output_path){
  
  age_less_18 <- data %>%
    filter(age < 18 |
             is.na(age))
  
  readmissions <- data %>%
    filter(age >= 18) %>%
    filter(readmission == "Yes")
  
  transferred_at_dis <- data %>%
    filter(age >= 18) %>%
    filter(!readmission %in% c("Yes")) %>%
    filter(hos_transfer_discharge == "Yes")
  
  
  if(type == 'hospital'){
    missing_status <- data %>%
      filter(age >= 18) %>%
      filter(!readmission %in% c("Yes")) %>%
      filter(!hos_transfer_discharge %in% c("Yes")) %>%
      filter(is.na(hos_dis_status))
    
    filtered_data <- data %>%
      filter(age >= 18 &
               !is.na(hos_dis_status) &
               !readmission %in% c("Yes") &
               !hos_transfer_discharge %in% c("Yes"))
    
  }else if (type == 'icu'){
    missing_status <- data %>%
      filter(age >= 18) %>%
      filter(!readmission %in% c("Yes")) %>%
      filter(!hos_transfer_discharge %in% c("Yes")) %>%
      filter(is.na(icu_dis_status))
    
    filtered_data <- data %>%
      filter(age >= 18 &
               !is.na(icu_dis_status) &
               !readmission %in% c("Yes") &
               !hos_transfer_discharge %in% c("Yes"))
  }
  
  output <- data.frame(matrix(ncol = 2, nrow = 0))
  columns <- c("Variable", "Value")
  colnames(output) <- columns
  
  output[1, ] <- c("Total ICU admissions during study period", nrow(data))
  output <- rbind(output, c("Number of admissions included in the analysis",
                            nrow(filtered_data)))
  output <- rbind(output, c("Total patients excluded",
                            nrow(data) - nrow(filtered_data)))
  output <- rbind(output, c("Age under 18 years",
                            nrow(age_less_18)))
  output <- rbind(output, c("Readmission to ICU within same hospital encounter",
                            nrow(readmissions)))
  output <- rbind(output, c("Interhospital transfer",
                            nrow(transferred_at_dis)))
  output <- rbind(output, c("Missing outcome",
                            nrow(missing_status)))
  
  
  output <- rbind(output, c("Median patient age (IQR)",
                            paste0(round(median(filtered_data$age, na.rm = TRUE)),
                                   ' (', round(quantile(filtered_data$age, 0.25, na.rm = TRUE)), '-',
                                   round(quantile(filtered_data$age, 0.75, na.rm = TRUE)), ')')))
  
  output <- rbind(output, c("Median systolic blood pressure (IQR)",
                            paste0(round(median(filtered_data$systolic_bp, na.rm = TRUE)),
                                   ' (', round(quantile(filtered_data$systolic_bp, 0.25, na.rm = TRUE)), '-',
                                   round(quantile(filtered_data$systolic_bp, 0.75, na.rm = TRUE)), ')')))
  
  output <- rbind(output, c("Number of included patients admitted after emergency surgery (% of total)",
                            paste0(nrow(filtered_data %>% filter(adm_type == "Emergency")), ' (',
                                   round(100*nrow(filtered_data %>% filter(adm_type == "Emergency"))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Number of included patients admitted after elective surgery (% of total)",
                            paste0(nrow(filtered_data %>% filter(adm_type == "Elective")), ' (',
                                   round(100*nrow(filtered_data %>% filter(adm_type == "Elective"))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Number of included patients receiving infused vasopressors/inotropes (% of total)",
                            paste0(nrow(filtered_data %>% filter(vasoactive == "Yes")), ' (',
                                   round(100*nrow(filtered_data %>% filter(vasoactive == "Yes"))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Number of included patients receiving invasive mechanical ventilation (% of total)",
                            paste0(nrow(filtered_data %>% filter(inv_vent == "Yes")), ' (',
                                   round(100*nrow(filtered_data %>% filter(inv_vent == "Yes"))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Number of patients receiving RRT (% of total)",
                            paste0(nrow(filtered_data %>% filter(renal_rt == "Yes")), ' (',
                                   round(100*nrow(filtered_data %>% filter(renal_rt == "Yes"))/nrow(filtered_data), 2), ')')))
  
  if (type == 'hospital'){
    output <- rbind(output, c("Total number of in-hospital deaths (% of total)",
                              paste0(nrow(filtered_data %>% filter(hos_dis_status == "Dead")), ' (',
                                     round(100*nrow(filtered_data %>% filter(hos_dis_status == "Dead"))/nrow(filtered_data), 2), ')'))) 
  } else if (type == 'icu') {
    output <- rbind(output, c("Total number of ICU deaths (% of total)",
                              paste0(nrow(filtered_data %>% filter(icu_dis_status == "Dead")), ' (',
                                     round(100*nrow(filtered_data %>% filter(icu_dis_status == "Dead"))/nrow(filtered_data), 2), ')')))
  }
  
  output <- rbind(output, c("Missing data for Blood pressure (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(systolic_bp))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(systolic_bp)))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Missing data for Metastasis/haematological malignancy (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(meta_haem))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(meta_haem)))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Missing data for Vasopressor/inotrope use (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(vasoactive))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(vasoactive)))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Missing data for Respiratory support (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(res_support))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(res_support)))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Missing data for Renal replacement therapy (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(renal_rt))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(renal_rt)))/nrow(filtered_data), 2), ')')))
  
  output <- rbind(output, c("Missing data for Admission type (% of total)",
                            paste0(nrow(filtered_data %>% filter(is.na(adm_type))), ' (',
                                   round(100*nrow(filtered_data %>% filter(is.na(adm_type)))/nrow(filtered_data), 2), ')')))
  
  
  write_csv(output, glue("{output_path}/pop_desc_for_{type}_mortality.csv"))
  
  filtered_data
}

define_complete_case <- function(data){
  
  data <- data %>%
    mutate(complete_case = if_else(
      !is.na(age) &
        !is.na(systolic_bp) &
        !is.na(adm_type) &
        !is.na(meta_haem) &
        !is.na(vasoactive) &
        !is.na(res_support) &
        !is.na(renal_rt), "Yes", "No"
    ))
  
  data
}

apply_imputation <- function(data){
  
  median_systolic_bp <- round(median(data$systolic_bp, na.rm = TRUE))
  mode_meta_haem <- Mode(data$meta_haem, na.rm = TRUE)[1]
  mode_vasoactive <- Mode(data$vasoactive, na.rm = TRUE)[1]
  mode_res_support <- Mode(data$res_support, na.rm = TRUE)[1]
  mode_renal_rt <- Mode(data$renal_rt, na.rm = TRUE)[1]
  mode_adm_type <- Mode(data$adm_type, na.rm = TRUE)[1]
  
  data <- data %>%
    mutate(systolic_bp = if_else(is.na(systolic_bp), median_systolic_bp, 
                                 systolic_bp),
           meta_haem = if_else(is.na(meta_haem), mode_meta_haem, meta_haem),
           vasoactive = if_else(is.na(vasoactive), mode_vasoactive, vasoactive),
           res_support = if_else(is.na(res_support), mode_res_support, 
                                 res_support),
           renal_rt = if_else(is.na(renal_rt), mode_renal_rt, renal_rt),
           adm_type = if_else(is.na(adm_type), mode_adm_type, adm_type))
  
  data
}

calculate_sms_icu <- function(data){
  
  data <- data %>%
    mutate(
      age_score = case_when(
        age <= 39 ~ 0L,
        age >= 40 & age <= 59 ~ 5L,
        age >= 60 & age <= 79 ~ 10L,
        age >= 80 ~ 13L
      ),
      sbp_score = case_when(
        systolic_bp <= 49 ~ 6L,
        systolic_bp >= 50 & systolic_bp <= 69 ~ 5L,
        systolic_bp >= 70 & systolic_bp <= 89 ~ 3L,
        systolic_bp >= 90 ~ 0L
      ),
      adm_type_score = if_else(
        adm_type == "Medical", 3L, 0L
      ),
      meta_haem_score = if_else(
        meta_haem == "Yes", 7L, 0L
      ),
      vaso_score = if_else(
        vasoactive == "Yes", 4L, 0L
      ),
      resp_score = if_else(
        res_support == "Yes", 5L, 0L
      ),
      rrt_score = if_else(
        renal_rt == "Yes", 4L, 0L
      ),
      sms_icu_score = age_score + sbp_score + adm_type_score + meta_haem_score + 
        vaso_score + resp_score + rrt_score,
      sms_icu_prob = exp(-4.80+0.21*sms_icu_score)/(1 + exp(-4.80+0.21*sms_icu_score))
      )
  
  data
}

generate_results <- function(data, output_path, patient_pop, type) {
  
  if(type == 'hospital'){
    data <- data %>%
      mutate(hos_dis_status_num = if_else(hos_dis_status == "Dead", 1L, 0L))
    
    actual_outcome <- data$hos_dis_status_num
    
  }else if (type == 'icu'){
    data <- data %>%
      mutate(icu_dis_status_num = if_else(icu_dis_status == "Dead", 1L, 0L))
    
    actual_outcome <- data$icu_dis_status_num
  }
  
  predicted_outcome <- data$sms_icu_prob 
  
  roc_obj <- pROC::roc(actual_outcome, predicted_outcome)
  
  auc_value <- pROC::auc(roc_obj)
  auc_ci <- pROC::ci.auc(roc_obj)
  se_auc <- sqrt(pROC::var(roc_obj)) 
  
  
  plot <- ggroc(roc_obj, colour = 'steelblue', size = 1.5, legacy.axes = TRUE) +
    ggtitle(paste0('ROC Curve ', '(AUC = ', round(auc_value[1], 2), ')')) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          title = element_text(size = 25)) +
    labs(x = "Specificity", y = 'Sensitivity') +
    geom_abline(linetype = "dotted", color = "grey", size = 1)
  
  png(filename = glue("{output_path}/roc_curve_{type}_mortality_{patient_pop}.png"), 
      width = 750, height = 650)
  
  print(plot)
  dev.off()
  
  
  png(filename = glue("{output_path}/calibration_curve_rms_{type}_mortality_{patient_pop}.png"),
      width = 1280, height = 650)
  
  curve_stat <- rms::val.prob(predicted_outcome, actual_outcome)
  dev.off()
  
  smr <- round(ems::SMR(actual_outcome, predicted_outcome), 2)
  smr_with_ci <- paste0(as.numeric(smr['SMR']), ' (',
                        as.numeric(smr['lower.Cl']), '-',
                        as.numeric(smr['upper.Cl']), ')')
  
  intercept <- as.numeric(curve_stat['Intercept'])
  slope <- as.numeric(curve_stat['Slope'])
  brier_score <- as.numeric(curve_stat['Brier'])
  
  output <- data.frame(matrix(ncol = 2, nrow = 0))
  columns <- c("Variable", "Value")
  colnames(output) <- columns
  
  output[1, ] <- c("Total patient encounters", nrow(data))
  output <- rbind(output, c("AUC score with 95% confidence Interval",
                            paste0(round(auc_value[1], 2), ' (', 
                                   round(auc_ci[1], 2), '-',
                                   round(auc_ci[3], 2), ')')))
  output <- rbind(output, c("Standard error of AUC score",
                            round(se_auc[1], 4)))
  
  output <- rbind(output, c("Gradient of calibration curve",
                            round(slope, 4)))
  output <- rbind(output, c("Intercept of calibration curve",
                            round(intercept, 4)))
  
  output <- rbind(output, c("O/E ratio (SMR)", smr_with_ci))
  
  output <- rbind(output, c("Brier score", round(brier_score, 4)))
  
  write_csv(output, glue("{output_path}/results_for_{type}_mortality_{patient_pop}.csv"))
}
