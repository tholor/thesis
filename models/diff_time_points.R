rm(list=ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
source("models\\helper_functions.R")
t_assess=c(0,1,2,3,4,5,6,7,8,9,10,11,12)
#t_assess = c(1)
#set.seed(23)
for(assessment_time in t_assess){
  #assessment_time = 6
  show(paste0("begin: model for  t= ",assessment_time))
  #______________________________________
  #SETTINGS
  #______________________________________
  use_existing_model="" #leave blank or enter file path to load model
  featureSelection = "elastic" #one of rfe, back, elastic, info_gain,manual
  discretizeLabs = FALSE #True/False
  convert_labs_diff = TRUE
  method= "pure_elastic" #rf, nb, log, pure_elastic
  elastic_hold_out = FALSE
  
  included_columns = c("age", "sex") #2x demographics
  included_columns = c(included_columns, "high_cci","hypertension","hypotension", "anemia", "pneumonia","diabetes_with_compl") # 6x diagnoses
  included_columns = c(included_columns, "ktv","tsat","hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #9xlabs
  included_columns = c(included_columns, "anticoagulants","statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #5x meds 
  included_columns = c(included_columns, "hypotension_problem","all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #7x complications
  included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #6x dialysis sessions
  included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #6x other
  
  comment = " "
  #included_columns = included_columns[!included_columns %in% c("hypotension_problem","tsat","ktv","anticoagulants")]
  #assessment_time = 1 #number of month
  outcome_time = assessment_time+12#number of month
  confusion_table = FALSE
  save_model= FALSE 
  standardize=FALSE #manual standardization of features
  aggregate = TRUE #combine some features to an aggegrated feature (listed in var_to_aggregate)
  var_to_aggregate = c("anti_hypertensive_med","cci") #which aggregated features should be used
  write_to_db = FALSE 
  topX = 10 # just applies to information_gain: use the Top X features
  old_input=FALSE
  write_coeffs = TRUE #writes coefficients and some summary to excel file (only for log. regression)
  FileExcelCoeffs = "output\\pure_elastic_50_times_last_period_obs_after_s.xlsx" # name of a file where the summary/coefficients should be saved
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
  df.preProcess = df.preProcess[complete.cases(df.preProcess),]
  if(standardize){df.preProcess = standardize_var(df.preProcess)}
  if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time,"log_at_1"))} 
  #variableImportance <- randomForest(outcome ~ ., data=select(df.preProcess, -pid), ntree=500, keep.forest=FALSE, importance=TRUE)
   #varImpPlot(variableImportance, sort = TRUE)
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
  
  if(use_existing_model!=""){ 
    #load existing model from file
    load(paste0(path,"\\models\\saved\\",use_existing_model,".R"))  
    #set train_data
    train_data = df.preProcess
  }else{
  # ________________________________
  # FEATURE SELECTION (Recursive feature elimination for Random Forest, naiveBayes)
  # ________________________________
  #variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
  #varImpPlot(variableImportance, sort = TRUE)
  #set.seed(21)
  subsets = c(seq(5,45,5))
  
  #RANDOM FOREST
  if(method == "rf"){
    rfFuncs2= rfFuncs
    rfFuncs2$summary = twoClassSummary
    ctrl= rfeControl(functions= rfFuncs2, method = "repeatedcv", repeats=10, number=5, allowParallel=TRUE)
  }
  #NAIVE BAYES
  if(method == "nb"){
    nbFuncs2=nbFuncs
    nbFuncs2$summary = twoClassSummary
    ctrl= rfeControl(functions= nbFuncs2, method = "repeatedcv", repeats=10, number=5, allowParallel=TRUE)
  }
  
  if(!method %in% c("log","pure_elastic")){
    featProfile =rfe(dplyr::select(df.preProcess,-outcome), df.preProcess$outcome ,sizes = subsets, rfeControl = ctrl, metric = "ROC")
    featProfile
    predictors(featProfile)
    featProfile$fit
    trellis.par.set(caretTheme())
    plot(featProfile, type = c("g", "o"))
    train_data = df.preProcess[, names(df.preProcess) %in% c(predictors(featProfile),"outcome")]
  }
  #________________________________
  # TRAIN MODEL 
  #________________________________
  #with caret (incl. internal validation with 10x 5-fold-cross-validation)
  tc = trainControl("repeatedcv",number=5,repeats=10, classProbs = TRUE, summaryFunction = twoClassSummary)
  
  #logistic regression with manually defined set of covariates
  if(method == "log" & featureSelection=="manual"){
    train_data = df.preProcess(names(df.preProcess) != "pid")
    logit_manual = train(outcome~age+low_pre_sbp_sitting, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    summary(logit_manual)
    curClassifier = logit_manual
    parameters = "none"
    AUC = curClassifier$results$ROC
  }
  #logistic regression with backward selection
  if(method == "log" & featureSelection=="back"){
    train_data = df.preProcess(names(df.preProcess) != "pid")
    #logit_mauri.out = train(outcome ~ age_10*cancer+sex+COPD_lung+liver_disease+access_type*cardiovascular+primary_renal_disease+access_type*bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #logit_mauri_impr.out = train(outcome ~ pain+creatinine+hypertension+potassium+albumin+ethnic+age_10*cancer+sex+COPD_lung+liver_disease+access_type*cardiovascular+primary_renal_disease+access_type*bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #take all variables and do backward feature selection
    logit_glm = glm(outcome ~ ., data=train_data, family=binomial(logit))
    back = step(logit_glm)  
    logit_back_auto = train(back$formula, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #logit_back(pretty good) = train(outcome ~ ethnic + cardiovascular + hypertension + primary_renal_disease + albumin + calcium + potassium + creatinine + pain_leg + pain_elsewhere + resulting_autonomy + age_10 + liver_disease + access_type, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    
    curClassifier = logit_back_auto
    parameters = "none"
    AUC = curClassifier$results$ROC
  }
  #logistic regression on 40% of data after deriving feature set from elastic net on 60% of data
  if(method == "log" & featureSelection=="elastic_split"){
    #use 60% of sample for feature selection with elastic net
    train_data = df.preProcess(names(df.preProcess) != "pid")
    if(elastic_hold_out == TRUE){
      sep_index = createDataPartition(train_data$outcome, p=0.6, list=FALSE, times=1)
      train_features_data = train_data[sep_index,]
      train_est_data = train_data[-sep_index,]
    } else{
      train_features_data = train_data
      train_est_data = train_data
    }
    #use 40% for training logistic regression with ML
    set.seed(23)
    features_input = model.matrix(~.,train_features_data[,!names(train_features_data) %in% "outcome"])
    logit_cv_glm = cv.glmnet(features_input,train_features_data$outcome, alpha=0.5, family="binomial", type.measure="auc", nfolds=5)
    plot(logit_cv_glm)
    coeff_out = coef(logit_cv_glm, s = "lambda.min")
    elastic_features = data.frame(coef.name = dimnames(coeff_out)[[1]], coef.value = matrix(coeff_out)) %>%
      subset(abs(coef.value) >0 & coef.name != "(Intercept)")
    elastic_formula = gsub("access_typefistula/graft_ready","access_type",
                           gsub("primary_renal_diseasediabetes|primary_renal_diseaseunknown|primary_renal_diseasehypertension|primary_renal_diseasesystemic|primary_renal_diseaseother","primary_renal_disease",
                                gsub("sexM","sex",
                                     gsub("resulting_autonomynormal|resulting_autonomylimited|resulting_autonomyspecial_care","resulting_autonomy",
                                     gsub("phosphoruslow","phosphorus",
                                     gsub("bmilow|bmihigh|bmivery high", "bmi",
                                gsub("1|limited|special care|Unknown|lower|higher|in_range|out_of_range","",elastic_features$coef.name))))))) %>%
      unique() %>%
      paste(collapse="+")
    elastic_formula = as.formula(paste0("outcome ~ ", elastic_formula))                          
    #elastic_formula = as.formula(paste0("outcome ~ ",paste(unique(gsub("1|limited|special care|Unknown|\\(Intercept\\)","",elastic_features$coef.name)),collapse="+")))
    logit_elastic = train(elastic_formula, data=train_est_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    curClassifier = logit_elastic
    parameters = "alpha = 0.5"
    AUC = curClassifier$results$ROC
  }
  #logistic regression after feature selection by elastic net (same sample)
  if(method == "log" & featureSelection=="elastic"){
  #use 60% of sample for feature selection with elastic net
  train_data = df.preProcess(names(df.preProcess) != "pid")
  if(elastic_hold_out == TRUE){
    sep_index = createDataPartition(train_data$outcome, p=0.6, list=FALSE, times=1)
    train_features_data = train_data[sep_index,]
    train_est_data = train_data[-sep_index,]
  } else{
    train_features_data = train_data
    train_est_data = train_data
  }
  #use 40% for training logistic regression with ML
  set.seed(23)
  features_input = model.matrix(~.,train_data[,!names(train_data) %in% "outcome"])
  logit_cv_glm = cv.glmnet(features_input,train_data$outcome, alpha=0.5, family="binomial", type.measure="auc", nfolds=5)
  plot(logit_cv_glm)
  coeff_out = coef(logit_cv_glm, s = "lambda.min")
  elastic_features = data.frame(coef.name = dimnames(coeff_out)[[1]], coef.value = matrix(coeff_out)) %>%
    subset(abs(coef.value) >0 & coef.name != "(Intercept)")
  elastic_formula = gsub("access_typefistula/graft_ready","access_type",
                         gsub("primary_renal_diseasediabetes|primary_renal_diseaseunknown|primary_renal_diseasehypertension|primary_renal_diseasesystemic|primary_renal_diseaseother","primary_renal_disease",
                              gsub("sexM","sex",
                                   gsub("resulting_autonomynormal|resulting_autonomylimited|resulting_autonomyspecial_care","resulting_autonomy",
                                        gsub("phosphoruslow","phosphorus",
                                             gsub("bmilow|bmihigh|bmivery high", "bmi",
                                                  gsub("1|limited|special care|Unknown|lower|higher|in_range|out_of_range","",elastic_features$coef.name))))))) %>%
    unique() %>%
    paste(collapse="+")
  elastic_formula = as.formula(paste0("outcome ~ ", elastic_formula))                          
  #elastic_formula = as.formula(paste0("outcome ~ ",paste(unique(gsub("1|limited|special care|Unknown|\\(Intercept\\)","",elastic_features$coef.name)),collapse="+")))
  logit_elastic = train(elastic_formula, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
  #     #diagnostics
  #     vif(logit_lasso_form$finalModel)
  #     outlierTest(logit_lasso_form$finalModel)
  #     cutoff <- 4/((nrow(train_data)-length(elastic_features)-2))
  #     plot(logit_lasso_form$finalModel, which=4, cook.levels=cutoff)
  #     plot(logit_lasso_form$finalModel)
  #     hoslem.test(logit_lasso_form$finalModel$y, fitted(logit_lasso_form$finalModel), g=5)
  #gbm_grid = expand.grid(lambda=seq(0.005,0.15, by=0.0145), alpha=c(0,0.5,1))
  #log_lasso = train(outcome ~ . , data = train_data, method="glmnet", family="binomial", trControl=tc, metric = "ROC")
  curClassifier = logit_elastic
  parameters = "alpha = 0.5"
  AUC = curClassifier$results$ROC
}
  #single penalized logistic regression with elastic net 
  if(method == "pure_elastic"){ #pure elastic net estimation
    train_data = df.preProcess[names(df.preProcess) != "pid"]
    AUCs = NULL
    for(i in 1:50){
      #set.seed(22)
      features_input = model.matrix(~.,train_data[,!names(train_data) %in% "outcome"])
      curClassifier = cv.glmnet(features_input,train_data$outcome, alpha=0.5, family="binomial", type.measure="auc", nfolds=5)
      #plot(curClassifier)
      coeff_out = coef(curClassifier, s = "lambda.min")
      if(!exists("AUCs")) {
        AUCs = curClassifier$cvm[curClassifier$lambda == curClassifier$lambda.min]
      }else{
        AUCs = c(AUCs, curClassifier$cvm[curClassifier$lambda == curClassifier$lambda.min])
      }
    }
    
    #predict 
    #all patients
    df.preProcess$prediction = predict(curClassifier, newx = features_input, s = "lambda.min", type = "response")
    pred = df.preProcess[names(df.preProcess) %in% c("pid", "prediction","outcome")]
    coeff_df = as.data.frame(data.frame(coef.name = dimnames(coeff_out)[[1]], coef.value = matrix(coeff_out)),row.names=NULL)
    
    #single patients of interest
    features_input_single = model.matrix(~.,subset(df.preProcess, pid == "LIB0000005201")[,!names(df.preProcess) %in% c("outcome","pid","prediction")])
    single_prediction = predict(curClassifier, newx = features_input_single, s = "lambda.min", type = "response")
    
    AUC = mean(AUCs)
    sdAUC = sd(AUCs)
    parameters = "alpha = 0.5"
} 
  #logistic regression after feature selection by information gain
  if(method == "log" & featureSelection=="info_gain"){
    #get features
    train_data = df.preProcess(names(df.preProcess) != "pid")
    infoGain =  information.gain(outcome~., train_data)
    infoGain = infoGain[order(infoGain$attr_importance,decreasing = TRUE),,drop=FALSE]
    infoGain = infoGain[1:topX,,drop=FALSE]
    info_formula = paste(row.names(infoGain), collapse="+")
    info_formula = as.formula(paste0("outcome ~ ", info_formula)) 
    logit = train(info_formula, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    curClassifier = logit
    parameters = paste0("topX = ",topX) 
    AUC = curClassifier$results$ROC
  }
  #random forest
  if(method == "rf"){
    newGrid = expand.grid(mtry = seq(2,10,1))
    rf_all.out = train(outcome ~ ., data=train_data, method="rf",trControl=tc, metric = "ROC", tuneGrid = newGrid)
    curClassifier = rf_all.out
    parameters= paste0("mtry",curClassifier$bestTune)
    AUC =  curClassifier$results[curClassifier$results$mtry==curClassifier$bestTune[1,1],2]
  }
  #support vector machine (linear)
  if(method =="svm"){
    svm.out = train(outcome ~ ., data=train_data, method="svmLinear",trControl=tc, metric = "ROC")
    curClassifier = svm.out
    parameters= "none"
    AUC =  curClassifier$results$ROC
  }
  #naive bayes
  if(method == "nb"){
    naive_markov.out = train(outcome ~ diabetes_with_compl+hypertension+hgb+calcXphosph+       potassium+           epogen_usage+       
                             resulting_autonomy+  liver_disease+  pain , data=train_data, method="nb",trControl=tc, metric = "ROC")
    naive_markov_gs.out = train(outcome ~ age+ cci+ hgb+ albumin+     calcXphosph+ potassium+ creatinine+ eGFR
                          , data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    naive_elastic_out = train(outcome ~ age+ethnic+cardiovascular+hypertension+dementia+primary_renal_disease+albumin+potassium+creatinine+epogen_usage+bp_problem+resulting_autonomy+liver_disease+access_type+pain,
                              data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    naive_elastic_no_ethnic = train(outcome ~ age+diabetes_with_compl+chf+cardiovascular+hypertension+primary_renal_disease+albumin+potassium+creatinine+epogen_usage+access_flow_poor+bp_problem+resulting_autonomy+liver_disease+access_type+pain,
                              data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    curClassifier = naive_elastic_out #change this line to select one of the different naive bayes classifier from above
    parameters = paste("LaPlace-correction=",naive_all.out$bestTune[1,1],", useKernel=",naive_all.out$bestTune[1,2])
    AUC =  curClassifier$results[curClassifier$results$fL==curClassifier$bestTune[1,1]&curClassifier$results$usekernel==curClassifier$bestTune[1,2],3]
  }
  }#end else "use existing model"
#________________________________
# Summarize results
#________________________________

  if(confusion_table | use_existing_model!=""){
    if(method=="log_elastic"){
      features_input = model.matrix(~.,df.preProcess[,!names(df.preProcess) %in% "outcome"])
      train_data$prob_dead = predict(curClassifier, features_input, s="lambda.min", type="response")
    }else{
      train_data$prob_dead = predict(curClassifier,train_data,type="prob")$Dead
    }
    myroc = pROC::roc(response=train_data$outcome, predictor=as.vector(train_data$prob_dead), auc=TRUE)
    plot(myroc, print.thres = "best")
    currentScore = as.numeric(pROC::auc(myroc))
    threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
    train_data$prediction = factor(ifelse(train_data$prob_dead > threshold, "Dead", "Alive") )
    ##Confusion Matrix 
    show(confusionMatrix(train_data$prediction,train_data$outcome, positive = "Dead"))
    if(use_existing_model!=""){
     AUC = currentScore
     parameters = ""
    }
  }
  
  summary(curClassifier)
  #plot(curClassifier)
  #write summary
  #write_results(curClassifier,AUC,discretizeLabs,df.preProcess,parameters,featureSelection,comment,outcome_time,assessment_time)
  #oldSummary = read.table(paste0(path,"\\output\\summary.csv"), sep=";", header = TRUE)
  #save model
  if(save_model){
    model_path=paste0(path,"\\models\\saved\\")
    model_name = paste0(method,"_",featureSelection,"_",as.character(Sys.Date()),"_",assessment_time,"_to_",outcome_time,"_",round(AUC*1000),".R")
    save(curClassifier,file=paste0(model_path,model_name))
  }
  #write features, coeffs and pvalues to csv
  #descriptive
  #temp = dplyr::select(df.preProcess,-c(age,potassium,creatinine,calcium,albumin,hgb,phosphorus, weight_gain,actual_duration))
  temp = dplyr::select(df.preProcess,-c(age,potassium,albumin,hgb))
  desc = gather(temp,variable, value) %>%
    count(variable, value)
  describeCohort = describe_cohort(train_data,"")
#________________________________
# Write results to spreadsheet
#________________________________
  if(write_coeffs){
    if(assessment_time==0){
      ifelse(method == "pure_elastic", file.copy("output\\template_elastic.xlsx", FileExcelCoeffs),file.copy("output\\template.xlsx", FileExcelCoeffs))
      write.xlsx(names(train_data), file=FileExcelCoeffs, sheetName="features", append=TRUE)
      all_features = c("")
    }
    if(method == "pure_elastic"){
      table = as.data.frame(data.frame(coef.name = dimnames(coeff_out)[[1]], coef.value = matrix(coeff_out)),row.names=NULL)
      all_features = unique(c(all_features,row.names(as.data.frame(rownames(coeff_out)))))
    }else{
      table = as.data.frame(summary(curClassifier)$coefficients)
      rownames(table) = gsub("1","",rownames(table)) #exclude "1" after name of binary covariates
      all_features = unique(c(all_features,row.names(as.data.frame(summary(curClassifier)$coefficients))))
    }
  table$AUC = AUC
  table$obs = nrow(train_data)
  table$deaths = nrow(train_data[train_data$outcome=="Dead",])
  table$num_features = nrow(subset(table, coef.value != 0))

  #all_features=unique(c(all_features,lapply(elastic_features$coef.name,as.character)))
  if(assessment_time ==max(t_assess)){
    write.xlsx(all_features, file=FileExcelCoeffs, sheetName="features_incl_levels", append=TRUE)
  }
  #show(paste("sheetname: t ",assessment_time," to ",outcome_time))
  write.xlsx(table, file=FileExcelCoeffs, sheetName=paste("t ",assessment_time," to ",outcome_time), append=TRUE)
  #write.xlsx(desc, file=FileExcelCoeffs, sheetName=paste("desc_cohort_",assessment_time), append=TRUE)
  write.xlsx(describeCohort, file=FileExcelCoeffs, sheetName=paste("desc_cohort_",assessment_time), append=TRUE) 
  }

  show(summary(curClassifier))
  
  #paste0("Number of deaths to predict: ",nrow(filter(train_data, outcome=="Dead")))
  #paste0("Mortality rate of : ",round(nrow(filter(train_data, outcome=="Dead"))/nrow(train_data),3))
  
  #logit_manual = train(outcome~age+high_cci+low_pre_sbp_sitting+high_pre_sbp_sitting+bmi, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
  #summary(logit_manual)
}