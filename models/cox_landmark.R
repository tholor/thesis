rm(list = ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
source("models\\helper_functions.R")
#______________________________________
#SETTINGS
#______________________________________
discretizeLabs = FALSE
convert_labs_diff = TRUE

included_columns = c("age", "sex") #demographics
included_columns = c(included_columns, "cci","hypertension","hypotension", "anemia", "pneumonia","diabetes_with_compl") # diagnoses
included_columns = c(included_columns, "hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #labs
included_columns = c(included_columns, "statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #meds 
included_columns = c(included_columns, "all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #complications
included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #dialysis sessions
included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #other

included_columns = c(included_columns, "ktv","anticoagulants","tsat","hypotension_problem") #other

#exclude not significant ones in log regression
included_columns = included_columns[!(included_columns %in% c("hypertension", "hypotension","calcXphosph", "epogen_usage",
                                                              "phosphorus","num_no_shows","num_time_decreased_by_pat",
                                                              "access_flow_poor","creatinine"))]
last_period = 36
standardize=FALSE
aggregate = TRUE
var_to_aggregate = c("anti_hypertensive_med")
write_to_db = FALSE
impute_death_period = FALSE
write_coeffs = FALSE
FileExcelCoeffs = "output\\cox\\coeffs_12_0_to_36_ktv_and_more.xlsx"
#______________________________________
# IMPORT DATA #
#______________________________________
df.import = query_data_cox(last_period)
#_____________________________________
# PREPROCESS
# ____________________________________

df.preProcess = convert_data_types_dyn(df.import)
if(discretizeLabs ){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
description_cohort_pre = describe_cohort(df.preProcess,"")
df.preProcess = df.preProcess[,(names(df.preProcess) %in% c(included_columns,"pid","start","stop","event"))]
#gather some information before excluding missing observations
bef_missing_excl = data.frame("num_obs" = as.data.frame(table(df.preProcess$start))$Freq, "mortality"=as.data.frame(table(filter(df.preProcess, event==1)$start))$Freq)
if(impute_death_period){df.preProcess = carry_forward(df.preProcess)}
df.preProcess_compl = df.preProcess #store all records for prediction

#exlude non-complete cases
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
if(standardize){df.preProcess = standardize_var(df.preProcess)}
if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time,"cox_at_1"))}
#df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
description_cohort_post = describe_cohort(df.preProcess,"")
description_cohort_post_at_1 = describe_cohort(filter(df.preProcess,start==1),"")
