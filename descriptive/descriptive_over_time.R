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
exclude_columns = c("anemia","pure_hypertension","diabetes_with_compl","access_problem","arterial_press_decreased","art_ven_pressure_increased","avg_epo_dose_per_kg","rel_weight_loss_desired","weight_gain","eGFR","fluid_removed","num_time_decreased_by_pat","rel_fluid_removed","days_hospitalized","planned_duration","infiltration_needle","headache","shortness_breath","num_no_shows","phosphorus","primary_renal_disease","access_problem","cci_hemmelgarn","nausea_vomiting","c_reactive_prot","vitamins","multiple_needle_sticks","bp_problem","start_year","access_flow_poor","pth","with_TMA_six","ktv","ethnic","ferritin","dead_first_year","adj_fdod","arterial_press_increased","venous_press_increased","connective_tissue","pain_chest","pain_elsewhere","pain_leg","hco3_bicarb","cholesterol","fully_followed")
exclude_columns = c(exclude_columns,"fever","loss_consciousness","weight_gain_excessive")
#exclude CCI diseases
exclude_columns = c(exclude_columns,"chf","any_tumor","metastatic_tumor","benign_or_uncertain_tumor","arthropathies","ulcer","COPD_lung","peripheral_artery_disease","cancer","dementia","cerebrovascular","copd_lung","liver_disease","infarct","hiv")
#exclude_columns = c(exclude_columns,"any_tumor","metastatic_tumor","benign_or_uncertain_tumor","arthropathies","ulcer","COPD_lung","cancer","dementia","copd_lung","liver_disease","hiv")
#exclude other comorbs:
exclude_columns = c(exclude_columns,"hepatitis_c","cardiovascular","hypotension","hypotension_hemodialysis","uncontrolled_hypertension","heart_diseases","diabetes_no_compl","hypertension","hemiplegia","lymphoma","leukemia","down_syndrome")
#exclude blood pressures
exclude_columns = c(exclude_columns,"pre_bp_diastolic_sitting","pre_bp_systolic_sitting","low_bp_and_chf","pre_pp_sitting","bp_low_problem","bp_high_problem")
#exclude because not significant
#exclude_columns = c(exclude_columns,"creatinine","fluid_removed","weight_gain","statins","with_TMA","num_time_decreased_by_pat","hgb","high_pre_sbp_sitting","phosphorus","infiltration_needle","arterial_press_decreased","calcXphosph","all_cramps","pain","art_ven_pressure_increased","avg_epo_dose_per_kg")
last_period = 24
standardize=FALSE
aggregate = TRUE
var_to_aggregate = c("anti_hypertensive_med")
impute_death_period = TRUE


#______________________________________
# IMPORT DATA #
#______________________________________
df.import = query_data_cox(last_period)
#_____________________________________
# PREPROCESS
# ____________________________________

df.preProcess = convert_data_types_dyn(df.import)
df.preProcess$avg_epo = df.import$avg_epo_dose_per_kg
if(discretizeLabs ){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
bef_missing_excl = as.data.frame(table(df.preProcess$start))
missing_counts = sapply(df.preProcess, function(y) sum(length(which(is.na(y)))))
if(impute_death_period){df.preProcess = carry_forward(df.preProcess)}
#df.preProcess = df.preProcess[complete.cases(df.preProcess),]
if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
if(standardize){df.preProcess = standardize_var(df.preProcess)}



library(data.table)
train_data =df.preProcess
#all observations per period
table(train_data$start)
#train_data$diff_duration = train_data$planned_duration-train_data$actual_duration
dt =data.table(train_data)
#plot(dt[,list(mean = mean(days_hospitalized)), by= start], xlab="time",ylab="% access= fistula")
# 
# #actual duration
# dt_duration = dt[,list(mean_duration = round(mean(actual_duration),3), sd_duration = round(sd(actual_duration),3)), by=start]
# dt_duration=dt_duration[order(start)]
boxplot(rel_weight_loss_desired~start, data=train_data, xlab= "Time", ylab="avg(rel_weight_loss_desired)")
filter(train_data, rel_weight_loss_desired> 0.1)

boxplot(hgb~start, data=train_data, xlab= "Time", ylab="avg(hgb)")
boxplot(rel_fluid_removed~start, data=train_data, xlab= "Time", ylab="avg(rel_fluid_removed)")
boxplot(avg_epo~start, data=train_data, xlab= "Time", ylab="avg(creatinine)")
hist(filter(train_data, start == 8)$avg_epo, breaks=60)

hist(filter(train_data, start == 6)$actual_duration, breaks=60)
after_missing_excl = as.data.frame(table(train_data$start))
  # 
# dt_planned = dt[,list(mean_duration = round(mean(diff_duration),3), sd_duration = round(sd(diff_duration),3)), by=start]
# dt_planned=dt_planned[order(start)]
# boxplot(num_hospitalizations~start, data=train_data, xlab= "Time", ylab="avg(diff_duration)")
#nrow(filter(train_data, start==1, access_type=="fistula/graft_ready"))/nrow(filter(train_data,start==1))
#filter(df.preProcess, pid == "LIB0000017458")
