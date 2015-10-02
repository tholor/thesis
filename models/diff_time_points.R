rm(list = ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
source("models\\helper_functions.R")
#______________________________________
#SETTINGS
#______________________________________
use_existing_model="" #leave blank or enter file path to load model
featureSelection = "elastic" #one of rfe, back, elastic, info_gain,manual
discretizeLabs = "False" #True/False
convert_labs_diff = "FALSE"
method= "log" #rf, nb, log
exclude_columns = c("primary_renal_disease","bp_problem","hypertension","start_year","access_flow_poor","bmi","with_TMA","with_TMA_six","ethnic","ferritin","ulcer","dead_first_year","calcium","adj_fdod","arterial_press_increased","pth","ktv","venous_press_increased","connective_tissue","pain_chest","pain_elsewhere","pain_leg","hemiplegia","lymphoma","leukemia","down_syndrome","hiv","hco3_bicarb","cholesterol","fully_followed")
#"cancer","chf","cardiovascular","copd_lung","liver_disease","hypertension","infarct","diabetes_with_compl","diabetes_no_compl",
exclude_columns = c(exclude_columns,"fever","loss_consciousness","weight_gain_excessive")
comment = "no ethnic, no bmi, no access_flow_poor, no start_year,with pure_hypertension"
assessment_time = 3#number of month
outcome_time =15#number of month
confusion_table = "FALSE"
save_model="FALSE"
standardize="FALSE"
aggregate = "FALSE"
write_to_db = "FALSE"
topX = 10 # just applies to information_gain: use the Top X features

#______________________________________
# IMPORT DATA #
#______________________________________
data_source= ifelse(assessment_time==0,"03_model_begin_to_1year","03_model_exact") #was "03_model_exact_temp4"
df.import = query_data_dyn(data_source,outcome_time,assessment_time)
#df.import = subset(df.import, period==assessment_time)
#_____________________________________
# PREPROCESS
# ____________________________________
if(assessment_time==0){ df.preProcess=convert_data_types(df.import) 
}else{df.preProcess = convert_data_types_dyn(df.import) }
if(discretizeLabs == "TRUE"){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff == "TRUE"){df.preProcess = convert_labs_to_diff(df.preProcess)}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
if(aggregate=="TRUE"){df.preProcess = aggregate_var(df.preProcess)}
if(standardize=="TRUE"){df.preProcess = standardize_var(df.preProcess)}
if(write_to_db=="TRUE"){write_to_database(df.preProcess, paste0("02_cohort_from_r_at_",assessment_time))}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% c("pid"))]
# variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
# varImpPlot(variableImportance, sort = TRUE)
#correlation matrix
# corData = df.preProcess[,!names(df.preProcess) %in% c("resulting_autonomy","primary_renal_disease","access_type")]
# corData$outcome = as.numeric(corData$outcome)
# corMat = as.data.frame(cor(corData))
# write.table(corMat, file = "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\correlation.csv", sep = ";")

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
  logit_manual = train(outcome~uncontrolled_hypertension, data= train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
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
                         gsub("primary_renal_diseasediabetes|primary_renal_diseasehypertension|primary_renal_diseasesystemic|primary_renal_diseaseother","primary_renal_disease",
                              gsub("sexM","sex",
                              gsub("1|limited|special care|Unknown","",elastic_features$coef.name)))) %>%
    unique() %>%
    paste(collapse="+")
  elastic_formula = as.formula(paste0("outcome ~ ", elastic_formula))                          
  #elastic_formula = as.formula(paste0("outcome ~ ",paste(unique(gsub("1|limited|special care|Unknown|\\(Intercept\\)","",elastic_features$coef.name)),collapse="+")))
  logit_lasso_form = train(elastic_formula, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
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

if(confusion_table == "TRUE" | use_existing_model!=""){
  train_data$prob_dead = predict(curClassifier,train_data,type="prob")$Dead
  myroc = pROC::roc(train_data$outcome, train_data$prob_dead,auc=TRUE)
  plot(myroc, print.thres = "best")
  currentScore = as.numeric(pROC::auc(myroc))
  threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  train_data$prediction = factor(ifelse(train_data$prob_dead > threshold, "Dead", "Alive") )
  ##Confusion Matrix 
  confusionMatrix(train_data$prediction,train_data$outcome, positive = "Alive")
  if(use_existing_model!=""){
   AUC = currentScore
   parameters = ""
  }
}

curClassifier
plot(curClassifier)
write_results(curClassifier,AUC,discretizeLabs,df.preProcess,parameters,featureSelection,comment,outcome_time,assessment_time)
#read summary
oldSummary = read.table(paste0(path,"\\output\\summary.csv"), sep=";", header = TRUE)

if(save_model==TRUE){
  model_path=paste0(path,"\\models\\saved\\")
  model_name = paste0(method,"_",featureSelection,"_",as.character(Sys.Date()),"_",assessment_time,"_to_",outcome_time,"_",round(AUC*1000),".R")
  save(curClassifier,file=paste0(model_path,model_name))
}

#descriptive
temp = dplyr::select(df.preProcess,-c(eGFR,age,potassium,creatinine,phosphorus,albumin,cci,hgb,calcXphosph))
desc = gather(temp,variable, value) %>%
  count(variable, value)
summary(curClassifier)

#plot(train_data$outcome,as.factor(train_data$cci), type ="h")
#plot(train_data$outcome, train_data$cci, type="h")
#qplot(cci, data = train_data, geom = "histogram", binwidth = 1, colour = outcome )
# 
# cor(as.numeric(df.preProcess$diabetes_no_compl), as.numeric(df.preProcess$outcome))
# cor(as.numeric(df.preProcess$diabetes_with_compl), as.numeric(df.preProcess$primary_renal_disease))
# 
# nrow(filter(df.preProcess, cci ==0 , outcome==1 ))/nrow(filter(df.preProcess, cci ==0))
# nrow(filter(df.preProcess, cci == 1, outcome==1 ))/nrow(filter(df.preProcess, cci == 1))
# nrow(filter(df.preProcess, cci == 2 , outcome==1 ))/nrow(filter(df.preProcess, cci == 2))
# nrow(filter(df.preProcess, cci == 3, outcome==1 ))/nrow(filter(df.preProcess, cci == 3))
# nrow(filter(df.preProcess, cci == 4, outcome==1 ))/nrow(filter(df.preProcess, cci == 4))
# nrow(filter(df.preProcess, cci == 5, outcome==1 ))/nrow(filter(df.preProcess, cci == 5))
# nrow(filter(df.preProcess, cci == 6, outcome==1 ))/nrow(filter(df.preProcess, cci == 6))
# nrow(filter(df.preProcess, cci >6, outcome==1 ))/nrow(filter(df.preProcess, cci > 6))
# 
# nrow(filter(df.preProcess, cci > 3, outcome==1 ))/nrow(filter(df.preProcess, cci > 3))
# nrow(filter(df.preProcess, cci <= 3, outcome==1 ))/nrow(filter(df.preProcess, cci <=3))





