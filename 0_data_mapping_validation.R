source('....../resources/functions_needed.R')


################################################################################
################# ATTENTION !!! ATTENTION !!! ATTENTION !!! ####################
################# ATTENTION !!! ATTENTION !!! ATTENTION !!! ####################
################################################################################
###### PLEASE MAKE SURE THAT MAPPED DATA HAS ONE ROW FOR ONE ADMISSION #########
###### PLEASE MAKE SURE THAT MAPPED DATA HAS ONE ROW FOR ONE ADMISSION #########
################################################################################
########### AVOID HAVING MORE THAN ONE ROW FOR THE SAME ADMISSION ##############
########### AVOID HAVING MORE THAN ONE ROW FOR THE SAME ADMISSION ##############
################################################################################

# Please give the data path
mapped_data <- read_csv('.....')

################################################################################
################################ STEP 1 ########################################
##### Checking all the expected columns are available in the mapped data #######

# Columns of interest 
columns_of_interest <- c('age', 'systolic_bp', 'meta_haem', 'vasoactive',
                         'res_support', 'renal_rt', 'adm_type',
                         'hos_dis_status', 'icu_dis_status',
                         'hos_transfer_admission', 'hos_transfer_discharge',
                         'readmission', 'inv_vent')

missing_columns <- setdiff(columns_of_interest, colnames(mapped_data))

if (length(missing_columns) > 0 ){
  print(paste("Following columns are missing in the mapped data: ",
              paste(missing_columns, collapse = ", ")))
}else{
  print("All the expected columns are available")
}


############ DON'T MOVE TO STEP 2 IF YOU HAVE ANY MISSING COLUMNS ##############
###### PLEASE ADD MISSING COLUMNS TO MAPPED DATA AND START FROM THE STEP 1 #####


################################################################################
################################ STEP 2 ########################################
### Making sure range of values/options for the variables in the mapped data ###


### Apply validation on variable types and options for non-numeric columns
validate_columns(mapped_data, columns_of_interest)


####### PLEASE CONTINUE ONLY IF YOU GET "All validations passed!" ##############

######## IF NOT, PLEASE REVISE THE MAPPING OF VARIABLES BASED ON ERRORS ########
################## AFTER REVISING, PLEASE START FROM STEP 1 ####################


### Make sure numeric column values are within the range and integers
### Imputing hospital outcome as Yes if icu outcome is Yes
### Imputing elective surgery as No if emergency surgery is Yes
### Imputing emergency surgery as No if elective surgery is Yes
mapped_data <- mapped_data %>%
  mutate(age = if_else(age >= 0 & age <= 125, round(age), as.numeric(NA), 
                       as.numeric(NA)),
         systolic_bp = if_else(systolic_bp >= 0 & systolic_bp <= 300,
                               round(systolic_bp), as.numeric(NA), as.numeric(NA)),
         hos_dis_status = if_else(icu_dis_status == "Dead", "Dead", 
                                  hos_dis_status, hos_dis_status))
