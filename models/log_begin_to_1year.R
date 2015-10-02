rm(list = ls())
gc()
setwd("C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R")
source("initialize_libraries.R")
source("models\\helper_functions.R")

#______________________________________
#SETTINGS
#______________________________________
data_source = "03_model_begin_to_1year" 
featureSelection = "elastic" #one of rfe, back, elastic
discretizeLabs = "False" #True/False
convert_labs_diff = "False"
method= "log" #rf, nb, log
exclude_columns = c("access_flow_poor","dead_first_year","start_year","adj_fdod","arterial_press_increased","bmi","ktv","venous_press_increased","connective_tissue","pain_chest","pain_elsewhere","pain_leg","hemiplegia","lymphoma","leukemia","down_syndrome","hiv")
comment = "age cont, w ethnic,no start_year, w/eGFR, no access_flow_poor"
outcome_time = 12 #6 months, 1 year, 3 months
confusion_table = "FALSE"
assessment_time = 0
#______________________________________
# IMPORT DATA #
#______________________________________
df.import = query_data_dyn(data_source,outcome_time,assessment_time)
#_____________________________________
# PREPROCESS
# ____________________________________
df.preProcess = convert_data_types(df.import) 
#df.preProcess$start_year = df.preProcess$start_year-2005
if(discretizeLabs == "True"){df.preProcess = discretize_labs(df.preProcess)}
if(convert_labs_diff == "True"){df.preProcess = convert_labs_to_diff(df.preProcess)}
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
df.preProcess = df.preProcess[complete.cases(df.preProcess),]
# ________________________________
#DESCRIPTIVE 
#_________________________________

# ________________________________
# FEATURE SELECTION
# ________________________________
 variableImportance <- randomForest(outcome ~ ., data=df.preProcess, ntree=500, keep.forest=FALSE, importance=TRUE)
 varImpPlot(variableImportance, sort = TRUE)
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
  featProfile =rfe(select(df.preProcess,-outcome), df.preProcess$outcome ,sizes = subsets, rfeControl = ctrl, metric = "ROC")
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
                         gsub("primary_renal_diseasediabetes","primary_renal_disease",
                         gsub("1|limited|special care|Unknown","",elastic_features$coef.name))) %>%
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
  naive_markov.out = train(outcome ~ ethnic+hypertension+pth+albumin+eGFR+ferritin+age+potassium+creatinine, data=train_data, method="nb",trControl=tc, metric = "ROC")
  naive_markov_gs.out = train(outcome ~ hgb+ albumin+ calcXphosph+ eGFR+pth+ potassium+ creatinine+ ferritin+ age_10
                              , data=train_data, method="nb",trControl=tc, metric = "ROC")
  
  naive_elastic_out = train(outcome ~ age+ethnic+cardiovascular+hypertension+dementia+ulcer+
                              primary_renal_disease+albumin+potassium+creatinine+bp_problem+resulting_autonomy+liver_disease+access_type+pain,
                            data=train_data, method="nb",trControl=tc, metric = "ROC")
 
  curClassifier = naive_all.out
  parameters = paste("LaPlace-correction=",naive_all.out$bestTune[1,1],", useKernel=",naive_all.out$bestTune[1,2])
  AUC =  curClassifier$results[curClassifier$results$fL==curClassifier$bestTune[1,1]&curClassifier$results$usekernel==curClassifier$bestTune[1,2],3]
}
curClassifier
plot(curClassifier)
write_results(curClassifier,AUC,discretizeLabs,df.preProcess,parameters,featureSelection,comment,outcome_time,"begin")
#read summary
oldSummary = read.table("C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\summary.csv", sep=";", header = TRUE)

if(confusion_table == "TRUE"){
  train_data$prob_dead = predict(curClassifier,train_data,type="prob")$Dead
  myroc = pROC::roc(train_data$outcome, train_data$prob_dead)
  plot(myroc, print.thres = "best")
  #currentScore = auc(myroc) 
  threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  train_data$prediction = factor(ifelse(train_data$prob_dead > threshold, "Dead", "Alive") )
  ##Confusion Matrix 
  confusionMatrix(predCut,train_data$outcome, positive = "Alive")
}
