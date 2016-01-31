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

included_columns = c("age", "sex") #demographics
included_columns = c(included_columns, "cci","hypertension","hypotension", "anemia", "pneumonia","diabetes_with_compl") # diagnoses
included_columns = c(included_columns, "hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #labs
included_columns = c(included_columns, "statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #meds 
included_columns = c(included_columns, "all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #complications
included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #dialysis sessions
included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #other

included_columns = c(included_columns, "ktv","anticoagulants","tsat","hypotension_problem") #other

#exclude not significant ones in log regression
included_columns = included_columns[!(included_columns %in% c("hypotension","calcXphosph",
                                                              "phosphorus","co2","num_no_shows","num_time_decreased_by_pat"))]
last_period = 12
standardize=FALSE
aggregate = TRUE
var_to_aggregate = c("anti_hypertensive_med")
write_to_db = FALSE
impute_death_period = FALSE
write_coeffs = FALSE
FileExcelCoeffs = "output\\cox\\coeffs_16_0_to_36_report.xlsx"
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
if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_cox_race"))}
#df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
description_cohort_post = describe_cohort(df.preProcess,"")
description_cohort_post_at_1 = describe_cohort(filter(df.preProcess,start==1),"")


#___________________________________________________________
# Model fitting
#___________________________________________________________
train_data = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
#surv_object = Surv(time=train_data$start,time2=train_data$stop,event=train_data$event)

#surv_object2 = Surv(train_data$start+1,train_data$event)
#coxph <- coxph(surv_object~ age+albumin, data = train_data)  
#cox <- coxph(Surv(time=start,time2=stop,event=event)~ ., data = train_data[,!names(train_data) %in% c("event","start","stop")], model = TRUE, method = "breslow")  
set.seed(50)
train_data$ageXt = train_data$age*train_data$start  
#train_data$ageXt = NULL
cox <- coxph(Surv(time=start,time2=stop,event=event)~ ., data = train_data, model = TRUE, method = "efron") 
cox_violated <- coxph(Surv(time=start,time2=stop,event=event)~ . -ageXt, data = train_data, model = TRUE, method = "efron")  
#cox <- coxph(Surv(time=start,time2=stop,event=event)~ tsat, data = train_data, model = TRUE, method = "efron")  
summary(cox_violated)
summary(cox)

#detection of influential obs. with residuals
#rr = resid(cox, type="dfbeta")
#plot(1:15283, rr[,8])
#r = as.data.frame(rr)
#rr[(rr[,8] < -0.02),8]
#cox <- coxph(Surv(time=start,time2=stop,event=event)~ sex+eGFR, data = train_data, model = TRUE, method = "efron")  
#cox
#plot(basehaz(cox))

#___________________________________________________________
# Performance evaluation with rms package
#___________________________________________________________
library(rms)
cox_rms = cph(Surv(start,stop,event)~ ., data = train_data[,!names(train_data) %in% c("ageXt")], x=T, y=T,surv=T, dxy=TRUE, time.inc=1, method = "efron")
print(cox_rms)
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

#___________________________________________________________
# Prediction with survfit for individual data
#___________________________________________________________
predict_cox = function(cox_model, ind_pid, df.preProcess_compl, last_period){
  test_data = filter(df.preProcess_compl,pid==ind_pid)
  last_obs = nrow(test_data)
  #fill up missing values inbetween    
   for(cur_row in which(!complete.cases(test_data))){
     show(cur_row)
     #get last observation values
     if(cur_row != 1){
        test_data[cur_row,is.na(test_data[cur_row,])] = test_data[cur_row-1,is.na(test_data[cur_row,])] 
     }
   }
  #fill up with values until "last period" of model
  if(last_obs<last_period){
  last_row = test_data[last_obs,]
  test_data[(last_obs+1):(last_period),]=last_row
  test_data[,c("start","stop")] = unique(train_data[order(train_data$start),c("start","stop")])
  }
 # show(test_data)
  return(summary(survfit(cox, newdata=test_data, id = pid)))
}
#predict_cox(cox,"LIB0000017458",df.preProcess_compl,last_period)

#___________________________________________________________
# #test for constant coefficients over time (PH assumption)
#___________________________________________________________
ph_test = cox.zph(cox, transform = "identity")
ph_test
 plot(ph_test[39])
abline(coef(cox)[c(1,38)], col = "red", lty=2)

#___________________________________________________________
# write results to file
#___________________________________________________________
if(write_coeffs){
  file.copy("output\\cox\\template.xlsx", FileExcelCoeffs)
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
  write.xlsx(description_cohort_pre, file=FileExcelCoeffs, sheetName="cohort_desc_pre", append=TRUE)
  write.xlsx(description_cohort_post, file=FileExcelCoeffs, sheetName="cohort_desc_post", append=TRUE)
  write.xlsx(description_cohort_post_at_1, file=FileExcelCoeffs, sheetName="cohort_desc_post_at_1", append=TRUE)
  write.xlsx(ph_test[[1]], file=FileExcelCoeffs, sheetName="PH-Test", append=TRUE)
  
}
# access_mortality= data.frame("period"= 1:last_period,
#                              "num_complete"= as.data.frame(table(train_data$start))$Freq,
#                              "mortality_complete"=as.data.frame(table(filter(train_data, event==1)$start))$Freq,
#                              "num_fistula_graft" = as.data.frame(table(filter(train_data, access_type=="fistula/graft_ready")$start))$Freq,
#                              "mortality_fistula_graft" =as.data.frame(table(filter(train_data, access_type=="fistula/graft_ready",event==1)$start))$Freq,
#                              "num_catheter" =as.data.frame(table(filter(train_data, access_type!="fistula/graft_ready")$start))$Freq
#                              )
# mortality_catheter =as.data.frame(table(filter(train_data, access_type!="fistula/graft_ready",event==1)$start))
# names(mortality_catheter) = c("period", "mortality_catheter")
# mortality_catheter$period = as.numeric(as.character(mortality_catheter$period))
# access_mortality=left_join(access_mortality, mortality_catheter, by="period")
# access_mortality$rate_all = round(access_mortality$mortality_complete/access_mortality$num_complete,3)
# access_mortality$rate_fistula = round(access_mortality$mortality_fistula_graft/access_mortality$num_fistula_graft,3)
# access_mortality$rate_catheter = round(access_mortality$mortality_catheter/access_mortality$num_catheter,3)
# 
#    corData = df.preProcess[,!names(df.preProcess) %in% c("primary_renal_disease","pre_edema_absent")]
#    corData$bmi = as.numeric(corData$bmi)-1
#    corData$resulting_autonomy = as.numeric(corData$resulting_autonomy)-1
#    corData$sex= as.numeric(corData$sex)-1
#    corData$access_type = as.numeric(corData$access_type)-1
#    corData = apply(corData,2, as.numeric)
#    corMat = as.data.frame(cor(corData))
#    write.table(corMat, file = "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\correlation_cohort_cox3.csv", sep = ";")
# 
# 
# aut = as.data.frame(table(train_data$start))
# aut$limited = as.data.frame(table(filter(train_data, resulting_autonomy == "limited")$start))$Freq
# aut$normal = as.data.frame(table(filter(train_data, resulting_autonomy == "normal")$start))$Freq
# aut$special = as.data.frame(table(filter(train_data, resulting_autonomy == "special care")$start))$Freq
# names(aut) = c("period","total","limited","normal","special_care")
# aut$perc_normal =  aut$normal /aut$total
# aut$perc_limited =  aut$limited/aut$total
# aut$perc_special_care = aut$special_care/aut$total
# 
# library(reshape)
# melted = melt(aut[,c(1,6,7,8)],id="period")
# ggplot(data=melted, aes(x=period, y=value, colour = variable, group=variable))+ geom_line()
