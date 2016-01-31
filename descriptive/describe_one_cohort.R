  rm(list=ls())
  gc()
  path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
  setwd(path)
  source("initialize_libraries.R")
  source("models\\helper_functions.R")
  assessment_time = 1
  outcome_time = 24
  #______________________________________
  #SETTINGS
  #______________________________________
  discretizeLabs = FALSE #True/False
  convert_labs_diff = FALSE
  
  included_columns = c("age", "sex") #2x demographics
  included_columns = c(included_columns, "high_cci","hypertension","hypotension", "anemia", "recent_pneumonia","diabetes_with_compl") # 6x diagnoses
  included_columns = c(included_columns, "ktv","tsat","hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #9xlabs
  included_columns = c(included_columns, "anticoagulants","statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #5x meds 
  included_columns = c(included_columns, "hypotension_problem","all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #7x complications
  included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #6x dialysis sessions
  included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #6x other
  
  comment = " "
  included_columns = included_columns[!included_columns %in% c("hypotension_problem","tsat")]
  #assessment_time = 1 #number of month
  standardize=FALSE #manual standardization of features
  aggregate = TRUE
  var_to_aggregate = c("anti_hypertensive_med","cci") #which variables should be aggregated
  old_input=FALSE
  write_coeffs = TRUE #writes coefficients and some summary to excel file (only for log. regression)
  FileExcelCoeffs = "output\\descriptive analyses\\all_patients_period_1.xlsx" # name of a file where the summary/coefficients should be saved
  #______________________________________
  # IMPORT DATA #
  #______________________________________
  data_source=ifelse(assessment_time==0 & old_input,"03_model_begin_to_1year","03_model_exact")
  df.import = query_data_dyn(data_source,outcome_time,assessment_time,old_input)
  #df.import = subset(df.import, period==assessment_time)
  #_____________________________________
  # PREPROCESS
  # ____________________________________
  if(assessment_time==0 && old_input){ df.preProcess=convert_data_types(df.import) 
  }else{df.preProcess = convert_data_types_dyn(df.import) }
  if(discretizeLabs){df.preProcess = discretize_labs(df.preProcess)}
  if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
  if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
  df.preProcess = df.preProcess[,(names(df.preProcess) %in% c(included_columns,"outcome", "pid"))]
  uncomplete = df.preProcess[!complete.cases(df.preProcess) & !is.na(df.preProcess$outcome),]
  subset
  df.preProcess = df.preProcess[complete.cases(df.preProcess),]
  if(standardize){df.preProcess = standardize_var(df.preProcess)}
  # variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
  # varImpPlot(variableImportance, sort = TRUE)
  #correlation matrix
  #   corData = df.preProcess[,!names(df.preProcess) %in% c("resulting_autonomy","primary_renal_disease","pre_edema_absent")]
  #    corData$bmi = as.numeric(corData$bmi)-1
  #    corData$sex= as.numeric(corData$sex)-1
  #    corData$access_type = as.numeric(corData$access_type)-1
  #    corData$outcome = as.numeric(corData$outcome)-1
  #    corData = apply(corData,2, as.numeric)
  #    corMat = as.data.frame(cor(corData))
  #   corrplot(cor(corData), method="number",  type = "lower", tl.cex=0.7, tl.col="black")
  #   
  #  write.table(corMat, file = "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\correlation_cohort_0_with_bmi_745.csv", sep = ";")

    

  #descriptive
  #temp = dplyr::select(df.preProcess,-c(age,potassium,creatinine,calcium,albumin,hgb,phosphorus, weight_gain,actual_duration))
  temp = dplyr::select(df.preProcess,-c(pid,age,potassium,albumin,hgb,ktv,calcium, creatinine, phosphorus,calcXphosph,actual_duration,weight_gain))
  desc = gather(temp,variable, value) %>%
    count(variable, value)
  describeCohort = describe_cohort(df.preProcess,"")
  #________________________________
  # Write results to spreadsheet
  #________________________________
    write.xlsx(describeCohort, file=FileExcelCoeffs, sheetName=paste("desc_cohort_",assessment_time), append=TRUE) 
 
  