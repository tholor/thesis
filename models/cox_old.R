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
comment = " "
confusion_table = FALSE
last_period = 24
save_model= FALSE
standardize=FALSE
aggregate = TRUE
var_to_aggregate = c("anti_hypertensive_med")
write_to_db = FALSE
impute_death_period = TRUE
write_coeffs = FALSE
FileExcelCoeffs = "output\\cox\\coeffs_2_0to36.xlsx"
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
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
bef_missing_excl = data.frame("num_obs" = as.data.frame(table(df.preProcess$start))$Freq, "mortality"=as.data.frame(table(filter(df.preProcess, event==1)$start))$Freq)
missing_counts = sapply(df.preProcess, function(y) sum(length(which(is.na(y)))))
if(impute_death_period){df.preProcess = carry_forward(df.preProcess)}
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
if(standardize){df.preProcess = standardize_var(df.preProcess)}
if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time))}
#df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]

#adjustments
#df.preProcess$hgb = log(df.preProcess$hgb/10)

#######MODEL###########

#train_data = df.preProcess[,names(df.preProcess) %in% c("age","albumin", "resulting_autonomy", "potassium", "hgb","start","stop","event")]
train_data = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
#surv_object = Surv(time=train_data$start,time2=train_data$stop,event=train_data$event)
#train_data$days_hospitalized = log(train_data$days_hospitalized)

#surv_object2 = Surv(train_data$start+1,train_data$event)
#coxph <- coxph(surv_object~ age+albumin, data = train_data)  
#cox <- coxph(Surv(time=start,time2=stop,event=event)~ ., data = train_data[,!names(train_data) %in% c("event","start","stop")], model = TRUE, method = "breslow")  
cox <- coxph(Surv(time=start,time2=stop,event=event)~ ., data = train_data, model = TRUE, method = "efron")  
#cox <- coxph(Surv(time=start,time2=stop,event=event)~ sex+eGFR, data = train_data, model = TRUE, method = "efron")  

#cox
summary(cox)
plot(basehaz(cox))

#with rms package
library(rms)
cox_rms = cph(Surv(start,stop,event)~ ., data = train_data, x=T, y=T,surv=T, dxy=TRUE, time.inc=1)
#do 10 times 5-fold CV
c_index = c(1:10)
for(i in 1:10){
  set.seed(NULL)
  eval = validate(cox_rms,method="crossvalidation", B=5, dxy=TRUE)
  Dxy=eval[1,5]
  c_index[i]= (Dxy/2)+0.5
}
avg_c_index = mean(c_index) #similar to AUC
#calibration
#c1 <- calibrate(cox_rms, u =1)  

######prediction with survfit for individual data
predict_cox = function(cox_model, ind_pid, df.preProcess, last_period){
  cox_model = cox
  ind_pid = "LIB0000017458"
  test_data = filter(df.preProcess,pid==ind_pid)
  #fill up with values until "last period" of model
  last_obs = nrow(test_data)
  if(last_obs<last_period){
  last_row = test_data[last_obs,]
  test_data[(last_obs+1):(last_period),]=last_row
  test_data[,c("start","stop")] = unique(train_data[order(train_data$start),c("start","stop")])}
 # show(test_data)
  return(summary(survfit(cox, newdata=test_data, id = pid)))
}
predict_cox(cox,"LIB0000017458",df.preProcess,last_period)

#predict(coxph,)
#with penalize package
# library(penalized)
# pen = optL1(surv_object, penalized = ~ age+albumin+phosphorus+resulting_autonomy+hgb+potassium, data = train_data, model="cox", fold=5)
# #basehaz(penalized_cox$fullfit)
# 
# #test for constant coefficients over time
test = cox.zph(cox)
 test
 plot(test[15])

#write results
if(write_coeffs){
  table_coeff = as.data.frame(summary(cox)$coefficients)
  table_coeff$sign = ifelse(table_coeff$"Pr(>|z|)"< 0.001, "***", 
                            ifelse(table_coeff$"Pr(>|z|)"< 0.01, "**",
                                   ifelse(table_coeff$"Pr(>|z|)"< 0.1,"*","")))
  table_summary = data.frame("periods" = unique(train_data$start),"num_obs"=bef_missing_excl$num_obs, 
                       "mortality" = bef_missing_excl$mortality,
                       "num_complete"= as.data.frame(table(train_data$start))$Freq, 
                       "mortality_complete"= as.data.frame(table(filter(train_data, event==1)$start))$Freq,"c_index" = avg_c_index)
  write.xlsx(names(train_data), file=FileExcelCoeffs, sheetName="features", append=TRUE)
  write.xlsx(table_coeff, file=FileExcelCoeffs, sheetName="cox_coeffs", append=TRUE)
  write.xlsx(table_summary, file=FileExcelCoeffs, sheetName="summary", append=TRUE)
  
}

