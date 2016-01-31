rm(list = ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
library(dynpred)
source("models\\helper_functions.R")
#______________________________________
#SETTINGS
#______________________________________
discretizeLabs = FALSE
convert_labs_diff = TRUE
# 
# included_columns = c("age", "sex") #demographics
# included_columns = c(included_columns, "cci","hypertension","hypotension", "anemia", "recent_pneumonia","diabetes_with_compl") # diagnoses
# included_columns = c(included_columns, "hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #labs
# included_columns = c(included_columns, "statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #meds 
# included_columns = c(included_columns, "all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #complications
# included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #dialysis sessions
# included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #other

#same features as used for elastic nets
included_columns = c("age", "sex") #2x demographics
included_columns = c(included_columns, "high_cci","hypertension","hypotension", "anemia", "pneumonia","diabetes_with_compl") # 6x diagnoses
included_columns = c(included_columns, "ktv","tsat","hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #9xlabs
included_columns = c(included_columns, "anticoagulants","statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #5x meds 
included_columns = c(included_columns, "hypotension_problem","all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #7x complications
included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #6x dialysis sessions
included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #6x other

#included_columns = c(included_columns, "ktv","anticoagulants","tsat","hypotension_problem") #other

# #exclude the ones not selected in any elastic net
# included_columns = included_columns[!(included_columns %in% c("hypotension","calcXphosph","phosphorus", "infiltration_needle","num_time_decreased_by_pat"))]
# 
# #exlude those with 1-2 occurences in elastic nets  
# included_columns = included_columns[!(included_columns %in% c("tsat", "creatinine", "weight_gain", "num_no_shows"))]
# 
# #exlude those with 3 occurences in elastic nets  
# included_columns = included_columns[!(included_columns %in% c("pneumonia","ktv","epogen_usage","statins","access_problem","all_cramps","high_pre_sbp_sitting"))]

included_columns = included_columns[!(included_columns %in% c("hypertension", "hypotension","calcXphosph", "epogen_usage",
                                                              "phosphorus","num_no_shows","num_time_decreased_by_pat",
                                                              "access_flow_poor","creatinine"))]

included_columns = included_columns[!(included_columns %in% c("infiltration_needle","calcium","statins","all_cramps","ktv","tsat"))]

fixed_vars = c("age","sex") #variables with fixed values for all t
varying_vars = included_columns[!included_columns %in% c(fixed_vars)] # variables with values depending on t
#varying_coeff_vars = c("shortness_breath", "beta_blockers","sex","weight_gain") # variables with coefficients depending on landmark
#varying_coeff_vars = c("sex","weight_gain", "albumin") # variables with coefficients depending on landmark
#varying_coeff_vars = c("sex","bmi") # variables with coefficients depending on landmark

window = 12 # number of periods that are predicted ahead P(survival > lm+window | survival > lm)
LMs <- seq(1,12,by=1) # Landmarks
last_period = 36
standardize=FALSE
aggregate = TRUE
var_to_aggregate = c("cci","anti_hypertensive_med")
write_to_db = FALSE
impute_death_period = FALSE
write_coeffs = TRUE
FileExcelCoeffs = "output\\cox\\landmarking\\02_only_vars_with_initally_good_performance_no_agg_cci.xlsx"
#______________________________________
# IMPORT DATA #
#______________________________________
df.import = query_data_landmark(last_period)
#_____________________________________
# PREPROCESS
# ____________________________________

df.preProcess = convert_data_types_dyn(df.import)
df.preProcess$period  = df.import$period
if(discretizeLabs ){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
#description_cohort_pre = describe_cohort(df.preProcess,"")
df.preProcess = df.preProcess[,(names(df.preProcess) %in% c(included_columns,"pid","status","survtime","period"))]
#gather some information before excluding missing observations
#bef_missing_excl = data.frame("num_obs" = as.data.frame(table(df.preProcess$start))$Freq, "mortality"=as.data.frame(table(filter(df.preProcess, event==1)$start))$Freq)
#if(impute_death_period){df.preProcess = carry_forward(df.preProcess)}
#df.preProcess_compl = df.preProcess #store all records for prediction

#exlude non-complete cases
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
#if(standardize){df.preProcess = standardize_var(df.preProcess)}
#if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time,"cox_at_1"))}
#df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
#description_cohort_post = describe_cohort(df.preProcess,"")
#description_cohort_post_at_1 = describe_cohort(filter(df.preProcess,start==1),"")
#_____________________________________
### prepare landmark data set
#_____________________________________
#test = df.preProcess[,names(df.preProcess) %in% c("age","sex","albumin","hgb", "cci", "resulting_autonomy", "bmi","survtime","status","period","pid")]

LMdata <- NULL
for (LM in LMs) {
  #TO DO: check again why there is sometimes period 0 associated with LM 1
  LMdataLM <- cutLM2(data=df.preProcess, outcome=list(time="survtime", status="status"),
                    LM=LM, horizon=LM+window, covs=list(fixed=fixed_vars,varying=varying_vars),
                    format="long", id="pid", rtime=c("period"), right = TRUE)
  LMdata <- rbind(LMdata,LMdataLM)
}
LMdata = subset(LMdata, LM == period+1)  #use covariates of period s-1 for landmark s 
LMdata = LMdata[complete.cases(LMdata),] #use only complete cases

#_____________________________________
# build model on complete data set
#_____________________________________
## Simple (ipl)

# #construct formula
 covs = paste(c(fixed_vars, varying_vars),collapse="+")
# formula = as.formula(paste0("Surv(LM,survtime,status) ~ ", covs, "+ strata(LM) + cluster(pid)"))  
# 
# #train model
# #LMsupercox0 <- coxph(Surv(LM,survtime,status) ~ age + albumin + cci + resulting_autonomy + bmi + strata(LM) + cluster(pid), data=LMdata, method="breslow")
# LMsupercox0 <- coxph(formula, data=LMdata, method="breslow")
# LMsupercox0

#varying coefficients (depending on landmark)
#prepare time functions
f2 <- function(t) (t/12)
#f3 <- function(t) (t/12)^2

# Explicitly code interactions of some variables with LM
interactions = ""
if(exists("varying_coeff_vars")){
  for(name in varying_coeff_vars){
    show(name)
    interaction_name = paste0(name,"_t")
    if(is.factor(LMdata[[name]])){
      if(nlevels(LMdata[[name]]) <= 2){
        LMdata[[interaction_name]] = (as.numeric(LMdata[[name]])-1)*f2(LMdata$LM)
      }else{
        for(i in 2:nlevels(LMdata[[name]])){ #multiple interaction terms are needed for factors with multiple categories 
          level_name = gsub(" ","",levels(LMdata[[name]])[i])
          LMdata[[paste0(name,"_",level_name,"_t")]] = ifelse(LMdata[[name]] == levels(LMdata[[name]])[i], f2(LMdata$LM),0)
          if(i == 2){
            interaction_name = paste0(name,"_",level_name,"_t")
          }else{
            interaction_name = paste0(interaction_name, "+",name,"_",level_name,"_t")        
          }
        }
      }
     # LMdata[[paste0(name,"_t2")]] = (as.numeric(LMdata[[name]])-1)*f3(LMdata$LM)
    }else{
      LMdata[[interaction_name]] = LMdata[[name]]*f2(LMdata$LM)
      #LMdata[[paste0(name,"_t2")]] = LMdata[[name]]*f3(LMdata$LM)
    }
    #interactions = paste0(interactions, "+",paste0(name,"_t"),"+",paste0(name,"_t2"))
    interactions = paste0(interactions, "+",interaction_name) 
  }
}
formula2 = as.formula(paste0("Surv(LM,survtime,status) ~ ", covs, interactions,"+ strata(LM) + cluster(pid)"))  

LMsupercox1 <- coxph(formula2, data=LMdata, method="breslow")
LMsupercox1

# #penalized cox
# features_input = model.matrix(~.,LMdata[,names(LMdata) %in% c(fixed_vars, varying_vars)])
# y = cbind(time=LMdata$timesurv, status = LMdata$status)
# coxPen = cv.glmnet(x= features_input, y = cbind(time=LMdata$survtime, status = LMdata$status), family = "cox",alpha=0.5, type.measure="deviance", nfolds=5)
# plot(coxPen)
# coeff_out = coef(coxPen, s = "lambda.1se")


#___________________________________________________________
# #test for constant coefficients over time (PH assumption)
#___________________________________________________________
ph_test = cox.zph(LMsupercox1, transform = "identity")
ph_test
for(i in 1:25)plot(ph_test[i])
abline(coef(LMsupercox1)[c(1,38)], col = "red", lty=2)
#____________________________
# bet <- LMsupercox1$coef
# sig <- LMsupercox1$var
# bet_comb = bet[grep("beta_blockers", names(bet))]
# 
# #m <- matrix(c(rep(1,length(LMs)),f2(LMs),f3(LMs)),length(LMs),3)
# m <- matrix(c(rep(1,length(LMs)),f2(LMs)),length(LMs),2)
# 
# LMsmooth <- data.frame(LM=LMs,logHR=as.numeric(m %*% bet_comb))
# plot(LMsmooth$LM,LMsmooth$logHR,type="l",lwd=2,lty=2,
#      xlim=c(0,12),
#      xlab="Time (years)",ylab="Log hazard ratio")


#predict
predictLM = function(cox, data, LM, window){
  sf = survfit(cox,  newdata = data)
  pred = summary(sf[LM,], time= LM+window)$surv
  data$prediction = 1-t(pred) #convert to P(death)
  return(data)
}

#_____________________________________
# Evaluate on same (complete) data set
#_____________________________________
AUC = data.frame(score = NA)
for(lm in LMs){
 # window = 12
  subsetLM = subset(LMdata, LM == lm)
  predictions = predictLM(cox = LMsupercox1, data = subsetLM, LM = LM, window = window)
  predictions$outcome = ifelse(predictions$status == 1, 1, ifelse(predictions$survtime == window+lm, 0, NA)) # 1 = survived
  predictions = predictions[complete.cases(predictions),] #remove patients, where outcome is unclear (censored within s+w)
  
  #evaluate
  myroc = pROC::roc(response=predictions$outcome, predictor=as.vector(predictions$prediction), auc=TRUE)
  AUC[lm,1] = as.numeric(pROC::auc(myroc))
  show(paste0("AUC at LM = ",lm, " : ",AUC[lm,1]))
}

#_____________________________________
# Evaluation with 5-fold-CV
#_____________________________________
AUC = data.frame(landmark = integer(), repetition = integer(), fold = integer(), score = double())
for( rep in 1:10){
    #prepare data
    obs = data.frame(unique(select(df.preProcess, pid, survtime, status)))
    fold = createFolds(obs$survtime, k=5)
    obs$fold = NA
    for(i in 1:5) 
      obs[fold[[i]],"fold"] = i
    #ddply(obs,~fold,summarise,mean=mean(survtime), meanStatus = mean(status)) #check if folds are similar
    LMdata_folded = merge(LMdata, select(obs, pid, fold), by= "pid") 
    
    for(curFold in 1:5){
      show (paste0("fold: ",curFold))
      train_data = subset(LMdata_folded,fold != curFold)
      test_data = subset(LMdata_folded,fold == curFold)
      #train model
      LMsupercox <- coxph(formula2, data=train_data, method="breslow")
      #predict
      for(lm in LMs){
        #window = 12
        subsetLM = subset(test_data, LM == lm)
        predictions = predictLM(cox = LMsupercox, data = subsetLM, LM = LM, window = window)
        predictions$outcome = ifelse(predictions$status == 1, 1, ifelse(predictions$survtime == window+lm, 0, NA)) # 1 = survived
        predictions = predictions[complete.cases(predictions),]
        
        #evaluate
        myroc = pROC::roc(response=predictions$outcome, predictor=as.vector(predictions$prediction), auc=TRUE)
        AUC = rbind(AUC, data.frame(landmark = lm,fold = curFold, repetition = rep, score = as.numeric(pROC::auc(myroc))))
       show(paste0("AUC at LM = ",lm, ", rep = ",rep," : ",as.numeric(pROC::auc(myroc))))
      }
    }
}
# get mean of AUCs
AUC_mean = ddply(AUC, ~ landmark, summarise, mean_auc = mean(score),sd_auc = sd(score))
AUC_mean
#___________________________________________________________
# write results to file
#___________________________________________________________
if(write_coeffs){
  file.copy("output\\cox\\landmarking\\template.xlsx", FileExcelCoeffs)
  table_coeff = as.data.frame(summary(LMsupercox1)$coefficients)
  table_coeff$sign = ifelse(table_coeff$"Pr(>|z|)"< 0.001, "***", 
                            ifelse(table_coeff$"Pr(>|z|)"< 0.01, "**",
                                   ifelse(table_coeff$"Pr(>|z|)"< 0.1,"*","")))
#   table_summary = data.frame("periods" = unique(train_data$LM),"num_obs"=bef_missing_excl$num_obs, 
#                              "mortality" = bef_missing_excl$mortality,
#                              "num_complete"= as.data.frame(table(train_data$start))$Freq, 
#                              "mortality_complete"= as.data.frame(table(filter(train_data, status==1)$start))$Freq,"c_index" = avg_c_index)
  write.xlsx(names(train_data), file=FileExcelCoeffs, sheetName="features", append=TRUE)
  write.xlsx(table_coeff, file=FileExcelCoeffs, sheetName="cox_coeffs", append=TRUE)
  #write.xlsx(table_summary, file=FileExcelCoeffs, sheetName="summary", append=TRUE)
  #write.xlsx(description_cohort_pre, file=FileExcelCoeffs, sheetName="cohort_desc_pre", append=TRUE)
  #write.xlsx(description_cohort_post, file=FileExcelCoeffs, sheetName="cohort_desc_post", append=TRUE)
  #write.xlsx(description_cohort_post_at_1, file=FileExcelCoeffs, sheetName="cohort_desc_post_at_1", append=TRUE)
  write.xlsx(ph_test[[1]], file=FileExcelCoeffs, sheetName="PH-Test", append=TRUE)
  write.xlsx(AUC, file=FileExcelCoeffs, sheetName="AUC", append=TRUE)
  write.xlsx(AUC_mean, file=FileExcelCoeffs, sheetName="AUC_mean", append=TRUE)
}
