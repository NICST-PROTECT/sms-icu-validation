################################################################################
# Give a path to a local directry to save the results csv
output_path <- "......."

#### Inclusion/Exclusion crieteria
data_for_hos_mortality <- inclusion_exclusion_criteria(mapped_data, 'hospital',
                                                       output_path)

#### Complete case 
data_for_hos_mortality <- define_complete_case(data_for_hos_mortality)

#### Imputation for missingness
data_for_hos_mortality <- apply_imputation(data_for_hos_mortality)

#### Calculation of SMS-ICU score and probability
data_for_hos_mortality <- calculate_sms_icu(data_for_hos_mortality)



################################################################################
############################ ANALYSIS ##########################################
################################################################################

#### Analysis on all admissions
generate_results(data_for_hos_mortality, output_path, 'all', 'hospital')

#### Analysis on all admissions (complete case)
data_for_hos_mortality_cc <- data_for_hos_mortality %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_cc, output_path, 'all(complete_case)', 
                 'hospital')


################################################################################
################################################################################

#### Analysis on medical admissions
data_for_hos_mortality_medical <- data_for_hos_mortality %>%
  filter(adm_type == "Medical")

generate_results(data_for_hos_mortality_medical, output_path, 'medical', 
                 'hospital')

#### Analysis on medical admissions (complete case)
data_for_hos_mortality_medical_cc <- data_for_hos_mortality_medical %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_medical_cc, output_path, 
                 'medical(complete_case)', 'hospital')


################################################################################
################################################################################

#### Analysis on emergency surgery admissions
data_for_hos_mortality_emergency <- data_for_hos_mortality %>%
  filter(adm_type == "Emergency")

generate_results(data_for_hos_mortality_emergency, output_path, 
                 'emergency_surgery', 'hospital')

#### Analysis on emergency surgery admissions (complete case)
data_for_hos_mortality_emergency_cc <- data_for_hos_mortality_emergency %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_emergency_cc, output_path, 
                 'emergency_surgery(complete_case)', 'hospital')


################################################################################
################################################################################

#### Analysis on scheduled surgery admissions
data_for_hos_mortality_elective <- data_for_hos_mortality %>%
  filter(adm_type == "Elective")

generate_results(data_for_hos_mortality_elective, output_path, 
                 'elective_surgery', 'hospital')

#### Analysis on scheduled surgery admissions (complete case)
data_for_hos_mortality_elective_cc <- data_for_hos_mortality_elective %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_elective_cc, output_path, 
                 'elective_surgery(complete_case)', 'hospital')


################################################################################
################################################################################

#### Analysis on admissions with IMV
data_for_hos_mortality_invasive <- data_for_hos_mortality %>%
  filter(inv_vent == "Yes")

generate_results(data_for_hos_mortality_invasive, output_path, 'invasive', 
                 'hospital')

#### Analysis on admissions with IMV (complete case)
data_for_hos_mortality_invasive_cc <- data_for_hos_mortality_invasive %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_invasive_cc, output_path, 
                 'invasive(complete_case)', 'hospital')


################################################################################
################################################################################

#### Analysis on admissions after excluding inter-hospital transfers
data_for_hos_mortality_transf_exc <- data_for_hos_mortality %>%
  filter(!hos_transfer_admission %in% c("Yes") &
           !hos_transfer_discharge %in% c("Yes"))

generate_results(data_for_hos_mortality_transf_exc, output_path, 
                 'inter_hos_transfers_excluded', 'hospital')

#### Analysis on admissions after excluding inter-hospital transfers
data_for_hos_mortality_transf_exc_cc <- data_for_hos_mortality_transf_exc %>%
  filter(complete_case == "Yes")

generate_results(data_for_hos_mortality_transf_exc_cc, output_path, 
                 'inter_hos_transfers_excluded(complete_case)', 'hospital')
