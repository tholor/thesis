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
convert_labs_diff = FALSE
exclude_columns = c("chf","primary_renal_disease","infiltration_needle","cci_hemmelgarn","nausea_vomiting","vitamins","multiple_needle_sticks","bp_problem","start_year","access_flow_poor","pth","ktv","with_TMA_six","ethnic","ferritin","dead_first_year","adj_fdod","arterial_press_increased","venous_press_increased","connective_tissue","pain_chest","pain_elsewhere","pain_leg","hco3_bicarb","cholesterol","fully_followed")
exclude_columns = c(exclude_columns,"fever","loss_consciousness","weight_gain_excessive")
#exclude CCI diseases
exclude_columns = c(exclude_columns,"any_tumor","metastatic_tumor","benign_or_uncertain_tumor","arthropathies","ulcer","COPD_lung","peripheral_artery_disease","cancer","dementia","cerebrovascular","copd_lung","liver_disease","infarct","hiv")
#exclude other comorbs:
exclude_columns = c(exclude_columns,"pure_hypertension","diabetes_with_compl","hepatitis_c","cardiovascular","hypotension","hypotension_hemodialysis","hypertension","uncontrolled_hypertension","heart_diseases","diabetes_no_compl","hypertension","hemiplegia","lymphoma","leukemia","down_syndrome")
#exclude blood pressures
exclude_columns = c(exclude_columns,"pre_bp_diastolic_sitting","pre_bp_systolic_sitting","low_bp_and_chf","pre_pp_sitting","bp_low_problem","bp_high_problem")
comment = " "
confusion_table = FALSE
assessment_time = 0 
save_model= FALSE
standardize=FALSE
aggregate = FALSE
write_to_db = FALSE


#______________________________________
# IMPORT DATA #
#______________________________________
df.import = query_data_cox_fixed(assessment_time)
#_____________________________________
# PREPROCESS
# ____________________________________

df.preProcess = convert_data_types_dyn(df.import)
if(discretizeLabs ){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
if(aggregate){df.preProcess = aggregate_var(df.preProcess)}
if(standardize){df.preProcess = standardize_var(df.preProcess)}
if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time))}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]

#######MODEL###########

#train_data = df.preProcess[,names(df.preProcess) %in% c("age","albumin", "resulting_autonomy", "potassium", "hgb","start","stop","event")]
train_data = df.preProcess
surv_object = Surv(time=train_data$start,time2=train_data$stop,event=train_data$event)
#surv_object2 = Surv(train_data$start+1,train_data$event)
#coxph <- coxph(surv_object~ age+albumin, data = train_data)  
coxph <- coxph(surv_object~ ., data = train_data[,!names(train_data) %in% c("event","start","stop")], model = TRUE, method = "breslow")  
coxph
summary(coxph)
#predict(coxph,)
#with penalize package
#library(penalized)
#pen = optL1(surv_object, penalized = ~ age+albumin+phosphorus+resulting_autonomy+hgb+potassium, data = train_data, model="cox", fold=5)
#basehaz(penalized_cox$fullfit)



test_data = filter(train_data[1,])
test_data$start = 0
test_data$stop = 
  plot(survfit(coxph, test_data, individual=TRUE))
test_data$start = 0
test_data$stop = 
  max(train_data$start)


