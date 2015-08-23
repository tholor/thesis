#IMPORT DATA
query_data = function(table,outcome_time){
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT * FROM ",table)
queried_data = dbGetQuery(con, strQuery)
#get correct outcome
strQuery = paste0("SELECT pid, period_of_death, last_period_observed FROM 01_death_periods")
queried_outcomes = dbGetQuery(con, strQuery)
#assess if patient is dead(=>1) within the horizon (outcome_time) or alive(=>0) or censored(=> NULL)
queried_outcomes$outcome = ifelse(period_of_death<outcome_time,1,
                              ifelse(period_of_death> outcome_time | last_observed> outcome_time,0,NULL))
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
  str_comorb_features = c("diabetes_no_compl", 
                          "diabetes_with_compl", 
                          "chf", 
                          "cardiovascular",
                          "peripheral_artery_disease",
                          "hypertension",
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
                          "benign_or_uncertain_tumor",
                          "cerebrovascular")
  
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
                         "weight_gain_excessive")
  
  #adjust feature definition 
  #data$age = data$age^2
  data$ethnic = ifelse(data$ethnic=="Unknown" | data$ethnic == "American Indian/Alaskan Native" | is.na(data$ethnic), "Unknown", data$ethnic )
  data$bmi = ifelse(data$bmi<20,"low","normal/high")
  data$liver_disease = ifelse(data$mild_liver_disease ==1 | data$severe_liver_disease==1, 1,0)
  data$access_type = ifelse(data$access_at_begin == 'fistula_usable' | data$access_at_begin == 'graft_usable', 'fistula/graft_ready', 'catheter_or_not_ready')
  data$primary_renal_disease = ifelse(is.na(data$primary_renal_disease),"other",data$primary_renal_disease)
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
  data = select(data, -c(pid,fully_followed,mild_liver_disease,severe_liver_disease,access_at_begin))
 #temp: adj_fdod removed from row above
 
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
  data$outcome[data$outcome== 0] = "Alive"
  data$outcome[data$outcome == 1] = "Dead"
  data[,"outcome"] = as.factor(data[,"outcome"])
#  data$dead_first_year = NULL 
#  data$dead_half_year = NULL 
#  data$dead_3_months = NULL 
return(data) 
}

#_______________________
# discretize lab values
# (from continous values to "in_range", "above","below")
# Attention: null values will automatically be converted so far!
#_______________________
discretize_labs = function(data){
  #data$albumin = as.factor(ifelse(data$albumin>4,"in_range",ifelse(data$albumin> 3.5, "low","very_low")))
  data$hgb = as.factor(ifelse((data$hgb >11| data$hgb  <10)&data$epogen_usage==1 | (data$hgb <12 &data$epogen_usage==0), "out_of_range","in_range")) # TO DO: check if 13 is correct
  data$calcium = as.factor(ifelse(data$calcium <8.4, "low",ifelse(data$calcium >9.5,"high","in_range")))
     #mutate(potassium = ifelse(albumin<3.7, "low","in_range")
  data$phosphorus = as.factor(ifelse(data$phosphorus<3.5, "low",ifelse(data$phosphorus>5.5,"high","in_range")))
  data$calcXphosph = as.factor(ifelse(data$calcXphosph>55, "high","in_range"))
  data$ferritin = as.factor(ifelse( data$ferritin<200 &data$epogen_usage==1, "low","in_range"))
    #mutate(creatinine = ifelse(albumin<3.7, "low","in_range")
  data$pth = as.factor(ifelse(data$pth<200, "low",ifelse(data$pth>300,"high","in_range")))
  return(data)
}
convert_labs_to_diff = function(data){
  #data$albumin = as.factor(ifelse(data$albumin>4,"in_range",ifelse(data$albumin> 3.5, "low","very_low")))
  #data$hgb = as.factor(ifelse((data$hgb >11| data$hgb  <10)&data$epogen_usage==1 | (data$hgb <12 &data$epogen_usage==0), "out_of_range","in_range")) # TO DO: check if 13 is correct
  data$calcium = abs(data$calcium-8.95)
  #mutate(potassium = ifelse(albumin<3.7, "low","in_range")
  data$phosphorus = abs(data$phosphorus-9)
  #data$calcXphosph = as.factor(ifelse(data$calcXphosph>55, "high","in_range"))
  #data$ferritin = as.factor(ifelse( data$ferritin<200 &data$epogen_usage==1, "low","in_range"))
  #mutate(creatinine = ifelse(albumin<3.7, "low","in_range")
  data$pth = abs(data$pth-250)
  return(data)
}

#__________________________________
#Write results of classifier to xlsx 
#__________________________________
write_results = function(curClassifier,AUC,discretizeLabs,df.preProcess, parameters,typeFeatureSelection,comment,outcome_time, assessment_time){
  #typeFeatureSelection = "RFE"
  #comment = "after 01_labs_closest_fdod"
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
  newRow = data.frame(time,outcome_time,assessment_time, comment, AUC, classifier,nObs,discretizeLabs, parameters,typeFeatureSelection, numFeaturesIn, featuresIn, numFeaturesSelected, featuresSelected, numFeaturesFinal, featuresFinal)
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


#NORMALIZATION??

#Markov blanket
#   learn.mb(df.preProcess, node="outcome",method = "gs" )
#   learn.mb(df.preProcess, node="outcome",method = "iamb" )
#   learn.nbr(df.preProcess, node="outcome",method = "mmpc")
#  learn.nbr(df.preProcess, node="outcome",method = "si.hiton.pc")

#learn BN 
#bn.gs = gs(df.preProcess)
#plot(bn.hc, main = "Constraint-based algorithms")
#bn.hc <- hc(df.preProcess)
# bn.tabu = tabu(df.preProcess)
# mb(bn.tabu,"outcome")
# graphviz.plot(bn.tabu)
# mb(bn.hc,"outcome")
# graphviz.plot(bn.hc)
