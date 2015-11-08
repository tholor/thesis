#IMPORT DATA
query_data_dyn = function(table,outcome_time,assessment_time, old_input){
  con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
  #get features
  if(assessment_time == 0 && old_input == "TRUE"){
    strQuery = paste0("SELECT * FROM ",table)
  }else{
  strQuery = paste0("SELECT * FROM ",table," WHERE period = ",assessment_time)
  }
  queried_data = dbGetQuery(con, strQuery)
  #get outcome
  strQuery = paste0("SELECT pid, period_of_death, last_period_observed FROM 01_death_periods")
  queried_outcomes = dbGetQuery(con, strQuery)
  #assess if patient is dead(=>1) within the horizon (outcome_time) or alive(=>0) or censored(=> NULL)
  queried_outcomes$outcome = ifelse(queried_outcomes$period_of_death<=outcome_time,1,
                                    ifelse(queried_outcomes$period_of_death>outcome_time,0,NA))
  queried_outcomes$outcome=ifelse(is.na(queried_outcomes$period_of_death) & queried_outcomes$last_period_observed>=outcome_time,0,queried_outcomes$outcome)
  queried_outcomes$period_of_death = NULL
  queried_outcomes$last_period_observed = NULL
  merged_data = merge(queried_data, queried_outcomes, by="pid")
  #close DB-Connection
  dbDisconnect(con)
  return(merged_data)
}

#_________________________________
# CONVERT DATA TYPES
# ________________________________
convert_data_types=function(data){
  data = df.import
  str_comorb_features = c("diabetes_no_compl", 
                          "diabetes_with_compl", 
                          "chf", 
                          "cardiovascular",
                          "peripheral_artery_disease",
                          "hypertension",
                          "pure_hypertension",
                          "hypotension_hemodialysis",
                          "COPD_lung",
                          "dementia",
                          "liver_disease",
                          "mild_liver_disease",
                          "severe_liver_disease",
                          "down_syndrome",
                          "arthropathies",
                          "hepatitis_c",
                          "connective_tissue",
                          "hiv",
                          "infarct",
                          "ulcer",
                          "hemiplegia",
                          "leukemia",
                          "lymphoma",
                          "cancer",
                          "benign_or_uncertain_tumor",
                          "cerebrovascular",
                          "any_tumor",
                          "metastatic_tumor")
  
  str_compl_features = c("access_problem",
                         "access_flow_poor",
                         "all_cramps",
                         "bp_problem",
                         "arterial_press_decreased",
                         "arterial_press_increased",
                         "multiple_needle_sticks",
                         "venous_press_increased",
                         "infiltration_needle",
                         "pain_chest",
                         "pain_leg",
                         "pain_elsewhere",
                         "nausea_vomiting",
                         "headache",
                         "fever",
                         "loss_consciousness",
                         "weight_gain_excessive",
                         "hypotension_problem")
  
  #adjust feature definition 
  #data$age = data$age^2
  data$ethnic = ifelse(data$ethnic=="Unknown" | is.na(data$ethnic), "Unknown", data$ethnic )
  data$ethnic = ifelse(data$ethnic == "American Indian/Alaskan Native" | data$ethnic == "Filipino" | data$ethnic == "Pacific Islander", "Other",data$ethnic) 
  data$bmi = ifelse(data$bmi<20,"low",ifelse(data$bmi<25,"normal",ifelse(data$bmi<35, "high", "very high")))
  data$liver_disease = ifelse(data$mild_liver_disease ==1 | data$severe_liver_disease==1, 1,0)
  data$access_type = ifelse(data$access_at_begin == 'fistula_usable' | data$access_at_begin == 'graft_usable', 'fistula/graft_ready', 'catheter_or_not_ready')
  data$primary_renal_disease = ifelse(is.na(data$primary_renal_disease),"unknown",data$primary_renal_disease)
  data$pain = ifelse(data$pain_chest==1 | data$pain_leg==1 |data$pain_elsewhere ==1, 1,0)
  data$art_ven_pressure_increased =  ifelse(data$arterial_press_increased ==1 | data$venous_press_increased==1, 1,0)
  data$start_year = as.numeric(substr(data$adj_fdod,1,4))
  #edit null values for comorbidities (null -> 0)
  replace_NAs = function(data){
    data[is.na(data)]=0
    return(data)
  }
 data[,names(data) %in% c(str_comorb_features,str_compl_features)] = apply(data[,names(data) %in% c(str_comorb_features,str_compl_features)],2, replace_NAs)
 
  #drop some columns
  data = dplyr::select(data,-c(fully_followed,access_at_begin))
 #temp: adj_fdod, pid removed from row above mild_liver_disease,severe_liver_disease
 
  #convert to factors:
  data[, names(data) %in% c("art_ven_pressure_increased","sex","bmi","ethnic","resulting_autonomy","dead_first_year","epogen_usage","access_type","pain",str_comorb_features,"primary_renal_disease",str_compl_features)] = 
    lapply(data[,names(data) %in% c("art_ven_pressure_increased","sex","bmi","ethnic","resulting_autonomy","dead_first_year","epogen_usage","access_type","pain",str_comorb_features,"primary_renal_disease",str_compl_features)], as.factor )

  #set standard level of factors
  data$resulting_autonomy = relevel(data$resulting_autonomy, ref = "normal")
  data$bmi = relevel(data$bmi, ref = "normal/high")
  data$primary_renal_disease = relevel(data$primary_renal_disease, ref = "other")
 
 #set outcome
#  if(outcome_type == "1 year"){
#    data$outcome[data$dead_first_year == 0] = "Alive"
#    data$outcome[data$dead_first_year == 1] = "Dead"
#  }
#  if(outcome_type == "6 months"){
#    data$outcome[data$dead_half_year== 0] = "Alive"
#    data$outcome[data$dead_half_year == 1] = "Dead"
#  }
#  if(outcome_type == "3 months"){
#    data$outcome[data$dead_3_months== 0] = "Alive"
#    data$outcome[data$dead_3_months == 1] = "Dead"
#  }
  #data$outcome[data$outcome== 0] = "Alive"
  #data$outcome[data$outcome == 1] = "Dead"
  data[,"outcome"] = as.factor(data[,"outcome"])
#  data$dead_first_year = NULL 
#  data$dead_half_year = NULL 
#  data$dead_3_months = NULL 
return(data) 
}

#_________________________________
# CONVERT DATA TYPES
# ________________________________
convert_data_types_dyn=function(data){
  data = df.import
  str_comorb_features = c("diabetes_no_compl", 
                          "diabetes_with_compl", 
                          "chf", 
                          "cardiovascular",
                          "peripheral_artery_disease",
                          "hypertension",
                          "pure_hypertension",
                          "hypotension",
                          "hypotension_hemodialysis",
                          "heart_diseases",
                          "pure_hypertension",
                          "COPD_lung",
                          "dementia",
                          "liver_disease",
                          "down_syndrome",
                          "arthropathies",
                          "hepatitis_c",
                          "connective_tissue",
                          "hiv",
                          "infarct",
                          "ulcer",
                          "hemiplegia",
                          "leukemia",
                          "lymphoma",
                          "cancer",
                          "any_tumor",
                          "metastatic_tumor",
                          "benign_or_uncertain_tumor",
                          "cerebrovascular",
                          "anemia",
                          "pneumonia")
  
  str_compl_features = c("access_problem",
                         "access_flow_poor",
                         "all_cramps",
                         "bp_problem",
                         "bp_high_problem",
                         "bp_low_problem",
                         "arterial_press_decreased",
                         "arterial_press_increased",
                         "multiple_needle_sticks",
                         "venous_press_increased",
                         "infiltration_needle",
                         "pain_chest",
                         "pain_leg",
                         "pain_elsewhere",
                         "nausea_vomiting",
                         "headache",
                         "fever",
                         "loss_consciousness",
                         "weight_gain_excessive",
                         "shortness_breath",
                         "hypotension_problem")
  
  str_med_features = c("statins","beta_blockers",
                       "ace_inhibitors","anemia","anticoagulants","bone_health","binders","antibiotics",
                       "asas","iron","vitamins","calcium_blockers","acid_pump","constipation","central_antihypertensive")
  
  #adjust feature definition 
  #data$age = data$age^2
  data$resulting_autonomy=ifelse(data$resulting_autonomy=="some_assistance","normal",data$resulting_autonomy) #only temporary solution
  data$ethnic = ifelse(data$ethnic=="Unknown" | is.na(data$ethnic)|data$ethnic == "American Indian/Alaskan Native" | data$ethnic == "Filipino" | data$ethnic == "Pacific Islander", "Unknown", data$ethnic )
  #data$ethnic = ifelse(data$ethnic == "American Indian/Alaskan Native" | data$ethnic == "Filipino" | data$ethnic == "Pacific Islander", "Other",data$ethnic) 
  data$bmi = ifelse(data$bmi<20,"low",ifelse(data$bmi<25,"normal",ifelse(data$bmi<35, "high", "very high")))
  #data$bmi = ifelse(data$bmi>20,"high/normal","low")
  data$liver_disease = ifelse(data$mild_liver_disease ==1 | data$severe_liver_disease==1, 1,0)
  #data$access_type = data$main_access_used
  data$access_type = ifelse(data$main_access_used == 'fistula' | data$main_access_used =='graft', 'fistula/graft_ready', data$main_access_used)
  data$access_type = ifelse(data$access_type == 'catheter', 'catheter_or_not_ready',   data$access_type)
  data$uncontrolled_hypertension= ifelse(data$pure_hypertension==1 & data$statins==0 & data$beta_blockers==0 & data$ace_inhibitors==0,1,0)
  data$primary_renal_disease = ifelse(is.na(data$primary_renal_disease),"unknown",data$primary_renal_disease)
  data$pain = ifelse(data$pain_chest==1 | data$pain_leg==1 |data$pain_elsewhere ==1, 1,0)
  data$art_ven_pressure_increased =  ifelse(data$arterial_press_increased ==1 | data$venous_press_increased==1, 1,0)
  data$high_pre_sbp_sitting = ifelse(data$pre_bp_systolic_sitting > 180, 1,0)
  data$low_pre_sbp_sitting = ifelse(data$pre_bp_systolic_sitting < 110, 1,0)
 # data$num_time_decreased_by_pat = ifelse(data$num_time_decreased_by_pat>0,1,0)
  #data$low_pre_pp_sitting = ifelse(data$pre_pp_sitting < 55, 1,0)
  data$low_bp_and_chf = ifelse(data$low_pre_sbp_sitting==1 & data$chf==1,1,0)
  #data$start_year = as.numeric(substr(data$adj_fdod,1,4))
  #edit null values for comorbidities etc. (null -> 0)
  replace_NAs = function(data){
    data[is.na(data)]=0
    return(data)
  }
  data[,names(data) %in% c(str_comorb_features,str_compl_features,str_med_features,"num_time_decreased_by_pat","num_hospitalizations","days_hospitalized","pain","art_ven_pressure_increased","num_no_shows","avg_epo_dose_per_kg")] = apply(data[,names(data) %in% c(str_comorb_features,str_compl_features,str_med_features,"num_hospitalizations","days_hospitalized","num_time_decreased_by_pat","pain","art_ven_pressure_increased","num_no_shows","avg_epo_dose_per_kg")],2, replace_NAs)
  
  data$avg_epo_dose_per_kg = as.factor(ifelse(data$avg_epo_dose_per_kg == 0, "none",ifelse(data$avg_epo_dose_per_kg < 150,"in_range","higher")))
  #drop some columns
  data = dplyr::select(data, -c(period,from_date,to_date,mild_liver_disease,severe_liver_disease,main_access_used))
  #temp: adj_fdod,pid removed from row above
  
  #convert to factors:
  data[, names(data) %in% c("with_TMA","high_pre_sbp_sitting","low_pre_sbp_sitting","pre_edema","avg_epo_dose_per_kg","art_ven_pressure_increased","sex","bmi","ethnic","resulting_autonomy","dead_first_year","epogen_usage","access_type","pain",str_comorb_features,str_med_features,"primary_renal_disease",str_compl_features)] = 
    lapply(data[,names(data) %in% c("with_TMA","high_pre_sbp_sitting","low_pre_sbp_sitting","pre_edema","avg_epo_dose_per_kg","art_ven_pressure_increased","sex","bmi","ethnic","resulting_autonomy","dead_first_year","epogen_usage","access_type","pain",str_comorb_features,str_med_features,"primary_renal_disease",str_compl_features)], as.factor )
  
  #set standard level of factors
  data$resulting_autonomy = relevel(data$resulting_autonomy, ref = "normal")
  data$bmi = relevel(data$bmi, ref = "high/normal")
  data$avg_epo_dose_per_kg = relevel(data$avg_epo_dose_per_kg, ref = "none")
  #data$primary_renal_disease = relevel(data$primary_renal_disease, ref = "other")


  if("outcome" %in% colnames(data)){
    data$outcome[data$outcome== 0] = "Alive"
    data$outcome[data$outcome == 1] = "Dead"
  data[,"outcome"] = as.factor(data[,"outcome"])}
  return(data) 
}

#_______________________
# discretize lab values
# (from continous values to "in_range", "above","below")
# Attention: null values will automatically be converted so far!
#_______________________
discretize_labs = function(data){
  #data$albumin = as.factor(ifelse(data$albumin>4,"in_range",ifelse(data$albumin> 3.5, "low","very_low")))
  #data$hgb = as.factor(ifelse((data$hgb >11| data$hgb  <10)&data$epogen_usage==1 | (data$hgb <11 &data$epogen_usage==0), "out_of_range","in_range"))
  data$hgb = as.factor(ifelse(data$hgb < 10, "lower","in_range"))
  data$calcium = as.factor(ifelse(data$calcium <8.4, "lower",ifelse(data$calcium >9.5,"higher","in_range")))
     #mutate(potassium = ifelse(albumin<3.7, "low","in_range")
  #data$phosphorus = as.factor(ifelse(data$phosphorus<3.5, "low",ifelse(data$phosphorus>5.5,"high","in_range")))
  data$calcXphosph = as.factor(ifelse(data$calcXphosph>55, "higher","in_range"))
  #data$ferritin = as.factor(ifelse( data$ferritin<200 &data$epogen_usage==1, "low","in_range"))
    #mutate(creatinine = ifelse(albumin<3.7, "low","in_range")
  #data$pth = as.factor(ifelse(data$pth<200, "low",ifelse(data$pth>300,"high","in_range")))
  return(data)
}
convert_labs_to_diff = function(data){
  #data$albumin = as.factor(ifelse(data$albumin>4,"in_range",ifelse(data$albumin> 3.5, "low","very_low")))
  #data$hgb = as.factor(ifelse((data$hgb >11| data$hgb  <10)&data$epogen_usage==1 | (data$hgb <12 &data$epogen_usage==0), "out_of_range","in_range")) # TO DO: check if 13 is correct
  data$calcium = abs(data$calcium-9.2)
  data$co2 = abs(data$co2-25.5) #recommended range of 22-29
  #mutate(potassium = ifelse(albumin<3.7, "low","in_range")
  #data$phosphorus = abs(data$phosphorus-4.5)
#  data$calcXphosph = ifelse(data$calcXphosph-55>0,data$calcXphosph-55,0)
  #data$ferritin = as.factor(ifelse( data$ferritin<200 &data$epogen_usage==1, "low","in_range"))
  #mutate(creatinine = ifelse(albumin<3.7, "low","in_range")
  #data$pth = abs(data$pth-250)
  return(data)
}

#______________
# standardize all variables 
#________________
standardize_var = function(data){
  data = df.preProcess
  #prepare binary variables
  for(i in 1:ncol(data)){
    if(is.factor(data[,i]) && !names(data)[i] %in% c("outcome","access_type","bmi","avg_epo_dose_per_kg") && nlevels(data[,i]) == 2){
      data[,i] = as.numeric(data[,i])-1
      data[,i]=(data[,i] + (1- mean(data[,i])))/2    
    }
  }
  #preparce ordinal variables
  if ("resulting_autonomy" %in% names(data)) {
    #convert to num
    data$resulting_autonomy = as.numeric(data$resulting_autonomy)-1
    lower_percentile=0
    for(i in 0:2){
      upper_percentile = lower_percentile+(nrow(subset(data, resulting_autonomy==i))/nrow(data))
      data$resulting_autonomy[data$resulting_autonomy==i] = (lower_percentile + upper_percentile) / 2
      lower_percentile = upper_percentile
    }
  }
  #scale to mean zero and unit stand. dev.
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i]) && !names(data)[i] %in% c("outcome","access_type","bmi","avg_epo_dose_per_kg"))data[,i]=scale(data[,i], center=TRUE, scale=TRUE)
  }
  return(data)
}
#__________________________________
# aggregate heart diseases  
# aggregate CCI: three levels (0,1-3, > 3)
#__________________________________
aggregate_var = function(data, var_to_aggregate){
  #heart diseases
  if("heart_diseases" %in% var_to_aggregate){
  data$heart_diseases = as.factor(ifelse(data$chf ==1 | data$cardiovascular ==1 | data$infarct == 1,1,0))
   data$chf = NULL
   data$cardiovascular = NULL
   data$infarct = NULL
  }
  if ("cci" %in% var_to_aggregate){
  #CCI
  #data$log_cci = ifelse(data$cci==0,0,log(data$cci))
  data$high_cci = as.factor(ifelse(data$cci<3,0,1))
  }
  if ("anti_hypertensive_med" %in% var_to_aggregate){
  #antihypertensive medication
  data$anti_hypertensive_med = as.factor(ifelse(data$ace_inhibitors==1 | data$calcium_blockers==1| data$central_antihypertensive==1,1,0 )) #
  data$ace_inhibitors = NULL
  data$calcium_blockers= NULL
  data$central_antihypertensive= NULL
  }
return(data)
}


#__________________________________
#Write results of classifier to xlsx 
#__________________________________
write_results = function(curClassifier,AUC,discretizeLabs,df.preProcess, parameters,typeFeatureSelection,comment,outcome_time, assessment_time){
  #typeFeatureSelection = "RFE"
  #comment = "test_comment"
  time = as.character(Sys.time())
  classifier = curClassifier$modelInfo$label
  nObs = nrow(curClassifier$trainingData)
  #parameters = paste0("mtry",curClassifier$bestTune)
  numFeaturesIn=ncol(df.preProcess)-1
  featuresIn = paste(colnames(df.preProcess[,names(df.preProcess) != "outcome"]), collapse = ';')
  numFeaturesSelected = ncol(curClassifier$trainingData)-1
  featuresSelected =   paste(colnames(curClassifier$trainingData[,names(curClassifier$trainingData) != ".outcome"]), collapse = ';')
  if(classifier== "Generalized Linear Model"){
    numFeaturesFinal = toString(nrow(coef(summary(curClassifier)))-1)
    featuresFinal = paste(rownames(coef(summary(curClassifier))), collapse = ';') 
  }else{
    numFeaturesFinal = "all previous"
    featuresFinal = "all previous"
  }  
  newRow = data.frame(time,assessment_time,outcome_time, comment, AUC, classifier,nObs,discretizeLabs, parameters,typeFeatureSelection, numFeaturesIn, featuresIn, numFeaturesSelected, featuresSelected, numFeaturesFinal, featuresFinal)
  oldSummary = read.table("C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\summary.csv", sep=";", header = TRUE)
  combinedSummary = rbind(oldSummary, newRow)
  write.table(combinedSummary, "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\summary.csv", sep=";",row.names = FALSE)
}

getOtherOutcome = function(data_in){
  data_in = df.preProcess
  con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
  strQuery = paste0("SELECT pid, dead_first_year, dead_half_year, dead_3_months
                      FROM prep_outcomes")
  queried_data = dbGetQuery(con, strQuery)
  merged_data = merge(data_in, queried_data, by="pid")
  dbDisconnect(con)
  return(outcome)
}

#__________________________________
#Write current cohort to DB 
#__________________________________
write_to_database = function(data,table_name){
  con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
  dbSendQuery(con, paste0("CREATE TABLE ", table_name," (pid VARCHAR(50));"))
  dbWriteTable(conn=con, value=data, name=table_name,overwrite = TRUE)
  #close DB-Connection
  dbDisconnect(con)
}
query_data_cox = function(last_period){
  con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
  #   if(assessment_time == 0){
  #     strQuery = paste0("SELECT * FROM ",table)
  #   }else{
  strQuery = paste0("SELECT model.*,
                    IF((model.period+1) = death.period_of_death, 1,0)as event
                    FROM 03_model_exact model INNER JOIN 01_death_periods death ON model.pid = death.pid 
                    WHERE model.period <= ", last_period," AND model.period > 0
                    AND (death.last_period_observed != model.period)")
  #}
  queried_data = dbGetQuery(con, strQuery)
  queried_data$start = queried_data$period
  queried_data$stop = queried_data$period+1
  #get correct outcome
  
  #close DB-Connection
  dbDisconnect(con)
  return(queried_data)
}

query_data_cox_fixed = function(assessment_time){
  con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
  #   if(assessment_time == 0){
  #     strQuery = paste0("SELECT * FROM ",table)
  #   }else{
  strQuery = paste0("SELECT model.*,
                    IF(model.period = death.period_of_death, 1,0) as event,
                    FROM 03_model_exact model INNER JOIN 01_death_periods death ON model.pid = death.pid WHERE model.period = ",assessment_time)
  #}
  queried_data = dbGetQuery(con, strQuery)
  queried_data$start = queried_data$period
  queried_data$stop = queried_data$period+1
  #get correct outcome
  
  #close DB-Connection
  dbDisconnect(con)
  return(queried_data)
}

# impute values in death period if missing (death might be very close to begin of period => no labs etc.)
carry_forward =function(data){
  #data = filter(df.preProcess, pid == "LIB0000014810" | pid =="LIB0000016449")
  #data = df.preProcess
  for(cur_row in which(data[,"event"]==1)){
  if(data[cur_row,"event"]==1 & !complete.cases(data[cur_row,])){
      #show(cur_row)
      #impute missing values, if period before death exists
      if(data[cur_row,"pid"]==data[cur_row-1,"pid"]){
        data[cur_row,is.na(data[cur_row,])] = data[cur_row-1,is.na(data[cur_row,])] 
      }
    }
  }
return(data)
}


describe_cohort = function(data, exclude_columns){
  data= data[, !names(data) %in% exclude_columns]
  #data = df.preProcess
  #missing data
  results = data.frame("feature"=names(data), "factor" = sapply(data, is.factor), "numeric" = sapply(data,is.numeric), "abs_missing" = sapply(data, function(y) sum(length(which(is.na(y))))))
  results$perc_missing = round(results$abs_missing/nrow(data),3)
  #mean etc for numeric
  cols_numeric = sapply(data, is.numeric)
  results[cols_numeric,"mean"] = sapply(data, mean, na.rm=TRUE)[cols_numeric]
  results[cols_numeric,"sd"] = apply(data,2, sd, na.rm=TRUE)[cols_numeric]
  results[cols_numeric,"max"] = apply(data,2, max, na.rm=TRUE)[cols_numeric]
  results[cols_numeric,"min"] = apply(data,2, min, na.rm=TRUE)[cols_numeric]
  # counting 1 for factors with 2 levels
  cols_factors_single = sapply(data, is.factor) & (sapply(data, nlevels)==2)
  results[cols_factors_single, "num_equals_lvl2"] = sapply(data, function(y) sum(length(which(y==levels(y)[2]))))[cols_factors_single]
  results[cols_factors_single, "perc_equals_lvl2"] = round(results$num_equals_lvl2/nrow(data),3)[cols_factors_single]
  #counting for factors with multiple levels
  cols_factors_multiple = sapply(data, is.factor) & (sapply(data, nlevels)>2)
  results[cols_factors_multiple, "obs_per_level"] = sapply(data, get_levels)[cols_factors_multiple]
  return(results)
}


#returns the number of observations per level of factor variable as one string
get_levels = function(column){
  if(is.factor(column)){
    df = as.data.frame(table(column))
    vect = toString(c(t((df))))}
  else{
    vect = ""
  }
  return(vect)
}


#NORMALIZATION??

# #Markov blanket
#   learn.mb(df.preProcess, node="outcome",method = "gs" )
#   learn.mb(df.preProcess, node="outcome",method = "iamb" )
#   learn.nbr(df.preProcess, node="outcome",method = "mmpc")
#  learn.nbr(df.preProcess, node="outcome",method = "si.hiton.pc")
# 
# #learn BN 
# bn.gs = gs(df.preProcess)
# plot(bn.hc, main = "Constraint-based algorithms")
# bn.hc <- hc(df.preProcess)
# bn.tabu = tabu(df.preProcess)
# mb(bn.tabu,"outcome")
# graphviz.plot(bn.tabu)
# mb(bn.hc,"outcome")
# graphviz.plot(bn.hc)

####

#plot(train_data$outcome,as.factor(train_data$cci), type ="h")
#plot(train_data$outcome, train_data$cci, type="h")
#qplot(pre_bp_systolic_sitting, data = train_data, geom = "histogram", binwidth = 1, colour = outcome )

# 
# cor(as.numeric(df.preProcess$diabetes_no_compl), as.numeric(df.preProcess$outcome))
# cor(as.numeric(df.preProcess$diabetes_with_compl), as.numeric(df.preProcess$primary_renal_disease))
# 
#nrow(filter(df.preProcess, cci ==0 , outcome==1 ))/nrow(filter(df.preProcess, cci ==0))
# nrow(filter(df.preProcess, cci == 1, outcome==1 ))/nrow(filter(df.preProcess, cci == 1))
# nrow(filter(df.preProcess, cci == 2 , outcome==1 ))/nrow(filter(df.preProcess, cci == 2))
# nrow(filter(df.preProcess, cci == 3, outcome==1 ))/nrow(filter(df.preProcess, cci == 3))
# nrow(filter(df.preProcess, cci == 4, outcome==1 ))/nrow(filter(df.preProcess, cci == 4))
# nrow(filter(df.preProcess, cci == 5, outcome==1 ))/nrow(filter(df.preProcess, cci == 5))
# nrow(filter(df.preProcess, cci == 6, outcome==1 ))/nrow(filter(df.preProcess, cci == 6))
# nrow(filter(df.preProcess, cci >6, outcome==1 ))/nrow(filter(df.preProcess, cci > 6))
# 
#nrow(filter(df.preProcess, chf ==1, outcome==1 ))/nrow(filter(df.preProcess, chf==1))
#  nrow(filter(df.preProcess, cci > 2, outcome==1 ))/nrow(filter(df.preProcess, cci >2))
# nrow(filter(df.preProcess, cci <= 2, outcome==1 ))/nrow(filter(df.preProcess, cci <=2))
