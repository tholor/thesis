rm(list=ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
source("models\\helper_functions.R")
t_assess=c(0,1,2,3,4,5,6,7,8,9,10,11,12)
set.seed(23)
for(assessment_time in t_assess){
  show(paste0("begin: model for  t= ",assessment_time))

  #______________________________________
  #SETTINGS
  #______________________________________
  use_existing_model="" #leave blank or enter file path to load model
  featureSelection = "elastic" #one of rfe, back, elastic, info_gain,manual
  discretizeLabs = FALSE #True/False
  convert_labs_diff = TRUE
  method= "log" #rf, nb, log
  
  included_columns = c("age", "sex") #demographics
  included_columns = c(included_columns, "high_cci","hypertension","hypotension", "anemia", "pneumonia","diabetes_with_compl") # diagnoses
  included_columns = c(included_columns, "ktv","tsat","hgb","albumin","calcium","phosphorus","calcXphosph","potassium","creatinine") #labs
  included_columns = c(included_columns, "anticoagulants","statins","epogen_usage", "beta_blockers","anti_hypertensive_med") #meds 
  included_columns = c(included_columns, "hypotension_problem","bp_falling","all_cramps","access_problem","access_flow_poor","shortness_breath","pain","infiltration_needle") #complications
  included_columns = c(included_columns, "actual_duration","num_time_decreased_by_pat", "weight_gain","pre_edema","low_pre_sbp_sitting","high_pre_sbp_sitting") #dialysis sessions
  included_columns = c(included_columns, "resulting_autonomy","num_hospitalizations","access_type","bmi","num_no_shows","with_TMA") #other
   #   exclude_columns = c("art_ven_pressure_increased","avg_epo_dose_per_kg","eGFR","arterial_press_decreased","days_hospitalized","planned_duration","fluid_removed","rel_fluid_removed","rel_weight_loss_desired","headache","primary_renal_disease","num_no_shows","cci_hemmelgarn","nausea_vomiting","c_reactive_prot","vitamins","multiple_needle_sticks","bp_problem","start_year","pth","ktv","with_TMA_six","ethnic","ferritin","dead_first_year","calcXphosph","adj_fdod","arterial_press_increased","venous_press_increased","connective_tissue","pain_chest","pain_elsewhere","pain_leg","hco3_bicarb","cholesterol","fully_followed")
#   exclude_columns = c(exclude_columns,"fever","loss_consciousness","weight_gain_excessive")
#   #exclude CCI diseases
#   exclude_columns = c(exclude_columns,"chf","any_tumor","metastatic_tumor","benign_or_uncertain_tumor","arthropathies","ulcer","COPD_lung","peripheral_artery_disease","cancer","dementia","cerebrovascular","copd_lung","liver_disease","infarct","hiv")
#   #exclude other comorbs:
#   exclude_columns = c(exclude_columns,"hepatitis_c","cardiovascular","hypotension_hemodialysis","hypertension","uncontrolled_hypertension","heart_diseases","diabetes_no_compl","hypertension","hemiplegia","lymphoma","leukemia","down_syndrome")
#   #,"pure_hypertension","diabetes_with_compl","hypotension"
  #exclude blood pressures
  #exclude_columns = c(exclude_columns,"pre_bp_diastolic_sitting","pre_bp_systolic_sitting","low_bp_and_chf","pre_pp_sitting")
  comment = " "
  assessment_time = 4 #number of month
  outcome_time = assessment_time+12#number of month
  confusion_table = TRUE
  save_model= FALSE
  standardize=TRUE
  aggregate = TRUE
  var_to_aggregate = c("anti_hypertensive_med","cci")
  write_to_db = FALSE
  topX = 10 # just applies to information_gain: use the Top X features
  old_input=FALSE
  write_coeffs = TRUE
  FileExcelCoeffs = "output\\model_coeffs_monthly_15_stand_hypo.xlsx"
  #______________________________________
  # IMPORT DATA #
  #______________________________________
  data_source=ifelse(assessment_time==0 & old_input,"03_model_begin_to_1year","03_model_exact")
  df.import = query_data_dyn(data_source,outcome_time,assessment_time,old_input)
  #df.import = subset(df.import, period==assessment_time)
  #_____________________________________
  # PREPROCESS
  # ____________________________________
  # if(assessment_time==0){ df.preProcess=convert_data_types(df.import) 
  # }else{df.preProcess = convert_data_types_dyn(df.import) }
  if(assessment_time==0 && old_input){ df.preProcess=convert_data_types(df.import) 
  }else{df.preProcess = convert_data_types_dyn(df.import) }
  if(discretizeLabs){df.preProcess = discretize_labs(df.preProcess)}
  if(convert_labs_diff){df.preProcess = convert_labs_to_diff(df.preProcess)}
  if(aggregate){df.preProcess = aggregate_var(df.preProcess,var_to_aggregate)}
  df.preProcess = df.preProcess[,(names(df.preProcess) %in% c(included_columns,"outcome"))]
  df.preProcess = df.preProcess[complete.cases(df.preProcess),]
  if(standardize){df.preProcess = standardize_var(df.preProcess)}
  if(write_to_db){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time,"log_at_1"))}
  df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
  # variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
  # varImpPlot(variableImportance, sort = TRUE)
  #correlation matrix
  #  corData = df.preProcess[,!names(df.preProcess) %in% c("resulting_autonomy","primary_renal_disease","pre_edema_absent")]
  #  corData$bmi = as.numeric(corData$bmi)-1
  #  corData$sex= as.numeric(corData$sex)-1
  #  corData$access_type = as.numeric(corData$access_type)-1
  #  corData$outcome = as.numeric(corData$outcome)-1
  #  corData = apply(corData,2, as.numeric)
  #  corMat = as.data.frame(cor(corData))
  #  write.table(corMat, file = "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\correlation_cohort_0_with_bmi_745.csv", sep = ";")
  
  if(use_existing_model!=""){
    #load model
    load(paste0(path,"\\models\\saved\\",use_existing_model,".R"))  
    #prepare train_data
    train_data = df.preProcess
  }else{
  # ________________________________
  # FEATURE SELECTION
  # ________________________________
  #variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
  #varImpPlot(variableImportance, sort = TRUE)
  set.seed(21)
  subsets = c(seq(5,45,5))
  
  #RANDOM FOREST
  if(method == "rf"){
    rfFuncs2= rfFuncs
    rfFuncs2$summary = twoClassSummary
    ctrl= rfeControl(functions= rfFuncs2, method = "repeatedcv", repeats=5, number=5, allowParallel=TRUE)
  }
  #NAIVE BAYES
  if(method == "nb"){
    nbFuncs2=nbFuncs
    nbFuncs2$summary = twoClassSummary
    ctrl= rfeControl(functions= nbFuncs2, method = "repeatedcv", repeats=5, number=5, allowParallel=TRUE)
  }
  
  if(!method %in% c("log")){
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
  
  if(method == "log" & featureSelection=="manual"){
    train_data = df.preProcess
    logit_manual = train(outcome~age+low_pre_sbp_sitting, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    summary(logit_manual)
    curClassifier = logit_manual
    parameters = "none"
    AUC = curClassifier$results$ROC
  }
  if(method == "log" & featureSelection=="back"){
    train_data = df.preProcess
    #logit_mauri.out = train(outcome ~ age_10*cancer+sex+COPD_lung+liver_disease+access_type*cardiovascular+primary_renal_disease+access_type*bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #logit_mauri_impr.out = train(outcome ~ pain+creatinine+hypertension+potassium+albumin+ethnic+age_10*cancer+sex+COPD_lung+liver_disease+access_type*cardiovascular+primary_renal_disease+access_type*bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    
    #logit_all.out = train(outcome ~ ., data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #take all variables and do backward feature selection
    logit_glm = glm(outcome ~ ., data=train_data, family=binomial(logit))
    back = step(logit_glm)  
    logit_back_auto = train(back$formula, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #logit_back(pretty good) = train(outcome ~ ethnic + cardiovascular + hypertension + primary_renal_disease + albumin + calcium + potassium + creatinine + pain_leg + pain_elsewhere + resulting_autonomy + age_10 + liver_disease + access_type, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    #logit_back_1_yr = train(outcome ~ ethnic + cardiovascular + hypertension + dementia + 
    #                      ulcer + primary_renal_disease + albumin + potassium + creatinine + 
    #                      epogen_usage + access_flow_poor + resulting_autonomy + age_10 + 
    #                      liver_disease + pain, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    
    # logit= train(outcome ~ ethnic+cardiovascular+hypertension+primary_renal_disease+albumin+calcium+calcXphosph+phosphorus+potassium+
    #                            creatinine+epogen_usage+bp_problem+pain+resulting_autonomy+
    #                            age_10+liver_disease, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
    curClassifier = logit_back_auto
    parameters = "none"
    AUC = curClassifier$results$ROC
  }
  if(method == "log" & featureSelection=="elastic"){
    train_data = df.preProcess
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
                                     gsub("bmilow|bmihigh", "bmi",
                                gsub("1|limited|special care|Unknown|lower|higher|in_range|out_of_range","",elastic_features$coef.name))))))) %>%
      unique() %>%
      paste(collapse="+")
    elastic_formula = as.formula(paste0("outcome ~ ", elastic_formula))                          
    #elastic_formula = as.formula(paste0("outcome ~ ",paste(unique(gsub("1|limited|special care|Unknown|\\(Intercept\\)","",elastic_features$coef.name)),collapse="+")))
    logit_lasso_form = train(elastic_formula, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
#     #diagnostics
#     vif(logit_lasso_form$finalModel)
#     outlierTest(logit_lasso_form$finalModel)
#     cutoff <- 4/((nrow(train_data)-length(elastic_features)-2))
#     plot(logit_lasso_form$finalModel, which=4, cook.levels=cutoff)
#     plot(logit_lasso_form$finalModel)
#     hoslem.test(logit_lasso_form$finalModel$y, fitted(logit_lasso_form$finalModel), g=5)
    #gbm_grid = expand.grid(lambda=seq(0.005,0.15, by=0.0145), alpha=c(0,0.5,1))
    #log_lasso = train(outcome ~ . , data = train_data, method="glmnet", family="binomial", trControl=tc, metric = "ROC")
    curClassifier = logit_lasso_form
    parameters = "alpha = 0.5"
    AUC = curClassifier$results$ROC
  }
  
  if(method == "log" & featureSelection=="info_gain"){
    #get features
    train_data = df.preProcess
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
  
  # neural= train(outcome  ~ ethnic + cardiovascular + hypertension + dementia + 
  #                 ulcer + primary_renal_disease + albumin + potassium + creatinine + 
  #                 epogen_usage + access_flow_poor + resulting_autonomy + age_10 + 
  #                 liver_disease + pain, data=train_data, method="nnet",trControl=tc,family=binomial(logit), metric = "ROC")
  
  
  if(method == "rf"){
    newGrid = expand.grid(mtry = seq(2,10,1))
    rf_all.out = train(outcome ~ ., data=train_data, method="rf",trControl=tc, metric = "ROC", tuneGrid = newGrid)
    curClassifier = rf_all.out
    parameters= paste0("mtry",curClassifier$bestTune)
    AUC =  curClassifier$results[curClassifier$results$mtry==curClassifier$bestTune[1,1],2]
  }
  
  if(method =="svm"){
    svm.out = train(outcome ~ ., data=train_data, method="svmLinear",trControl=tc, metric = "ROC")
    curClassifier = svm.out
    parameters= "none"
    AUC =  curClassifier$results$ROC
  }
  if(method == "nb"){
    naive_markov.out = train(outcome ~ diabetes_with_compl+hypertension+hgb+calcXphosph+       potassium+           epogen_usage+       
                             resulting_autonomy+  liver_disease+  pain , data=train_data, method="nb",trControl=tc, metric = "ROC")
    naive_markov_gs.out = train(outcome ~ age+ cci+ hgb+ albumin+     calcXphosph+ potassium+ creatinine+ eGFR
                          , data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    naive_elastic_out = train(outcome ~ age+ethnic+cardiovascular+hypertension+dementia+primary_renal_disease+albumin+potassium+creatinine+epogen_usage+bp_problem+resulting_autonomy+liver_disease+access_type+pain,
                              data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    naive_elastic_no_ethnic = train(outcome ~ age+diabetes_with_compl+chf+cardiovascular+hypertension+primary_renal_disease+albumin+potassium+creatinine+epogen_usage+access_flow_poor+bp_problem+resulting_autonomy+liver_disease+access_type+pain,
                              data=train_data, method="nb",trControl=tc, metric = "ROC")
    
    curClassifier = naive_elastic_out
    parameters = paste("LaPlace-correction=",naive_all.out$bestTune[1,1],", useKernel=",naive_all.out$bestTune[1,2])
    AUC =  curClassifier$results[curClassifier$results$fL==curClassifier$bestTune[1,1]&curClassifier$results$usekernel==curClassifier$bestTune[1,2],3]
  }
  }#end else "use existing model"
  
  if(confusion_table | use_existing_model!=""){
    train_data$prob_dead = predict(curClassifier,train_data,type="prob")$Dead
    myroc = pROC::roc(train_data$outcome, train_data$prob_dead,auc=TRUE)
    plot(myroc, print.thres = "best")
    currentScore = as.numeric(pROC::auc(myroc))
    threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
    train_data$prediction = factor(ifelse(train_data$prob_dead > threshold, "Dead", "Alive") )
    ##Confusion Matrix 
    show(confusionMatrix(train_data$prediction,train_data$outcome, positive = "Alive"))
    if(use_existing_model!=""){
     AUC = currentScore
     parameters = ""
    }
  }
  
  summary(curClassifier)
  #plot(curClassifier)
  #write summary
  write_results(curClassifier,AUC,discretizeLabs,df.preProcess,parameters,featureSelection,comment,outcome_time,assessment_time)
  oldSummary = read.table(paste0(path,"\\output\\summary.csv"), sep=";", header = TRUE)
  #save model
  if(save_model){
    model_path=paste0(path,"\\models\\saved\\")
    model_name = paste0(method,"_",featureSelection,"_",as.character(Sys.Date()),"_",assessment_time,"_to_",outcome_time,"_",round(AUC*1000),".R")
    save(curClassifier,file=paste0(model_path,model_name))
  }
  #write features, coeffs and pvalues to csv
  #descriptive
  temp = dplyr::select(df.preProcess,-c(age,potassium,creatinine,calcium,albumin,hgb,phosphorus, weight_gain,actual_duration))
  desc = gather(temp,variable, value) %>%
    count(variable, value)
  describeCohort = describe_cohort(train_data,"")
  
  if(write_coeffs){
  table = as.data.frame(summary(curClassifier)$coefficients)
  table$AUC = AUC
  table$obs = nrow(train_data)
  table$deaths = nrow(train_data[train_data$outcome=="Dead",])
  
  if(assessment_time==0){
    file.copy("output\\template.xlsx", FileExcelCoeffs)
    write.xlsx(names(train_data), file=FileExcelCoeffs, sheetName="features", append=TRUE)
    all_features = c("")
  }
  #all_features=unique(c(all_features,lapply(elastic_features$coef.name,as.character)))
  all_features = unique(c(all_features,row.names(as.data.frame(summary(curClassifier)$coefficients)) ))
  if(assessment_time ==max(t_assess)){
    write.xlsx(all_features, file=FileExcelCoeffs, sheetName="features_incl_levels", append=TRUE)
  }
  show(paste("sheetname: t ",assessment_time," to ",outcome_time))
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