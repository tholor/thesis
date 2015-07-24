rm(list = ls())
gc()
setwd("C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R")
#library(data.table)
library(RMySQL)
library(DBI)
#library(tables)
library(plyr)
library(dplyr)
library(GGally)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(rJava)
library(xlsx)
library(tabplot)
library(Hmisc)
#library(extremevalues)
#library(lubridate)
library(ggplot2)
library(caret)
#library(randomForest)
library(RANN)
library(e1071)
library(pROC)
#strQuery = paste0("SELECT ROUND((age/10),0) as age_10, sex, copd_lung, cancer, GREATEST(mild_liver_disease, severe_liver_disease) as liver_disease, cardiovascular,
 #                 (CASE WHEN access_at_begin = 'fistula_usable' OR access_at_begin = 'fistula_installed' OR access_at_begin = 'graft_installed' OR access_at_begin = 'graft_usable' THEN 'fistula/graft' ELSE 'catheter' END)as access_type,
  #                (CASE WHEN bmi < 20 THEN 'low' ELSE 'normal/high' END) AS bmi, resulting_autonomy, albumin, creatinine, cci,dementia, diabetes_with_compl,peripheral_artery_disease, dead_first_year FROM 03_model_begin_to_1year")

#import data
#currently missing: CKD reason AND grade of functional autonomy
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT ROUND((age/10),0) as age_10, copd_lung, cancer, GREATEST(mild_liver_disease, severe_liver_disease) as liver_disease, cardiovascular,
                  (CASE WHEN access_at_begin = 'fistula_usable' OR access_at_begin = 'fistula_installed' OR access_at_begin = 'graft_installed' OR access_at_begin = 'graft_usable' THEN 'fistula/graft' ELSE 'catheter' END)as access_type,
                  (CASE WHEN bmi < 20 THEN 'low' ELSE 'normal/high' END) AS bmi, resulting_autonomy, albumin, creatinine, cci,dementia, diabetes_with_compl, dead_first_year FROM 03_model_begin_to_1year")

df.import = dbGetQuery(con, strQuery)
#write.xlsx(df.import, "test.xlsx")
#close DB-Connection
dbDisconnect(con)

#preprocess data
str(df.import)
df.import[, names(df.import) %in% c("sex", "copd_lung", "cancer", "liver_disease", "cardiovascular", "access_type", "bmi","resulting_autonomy","diabetes_with_compl","dementia","peripheral_artery_disease", "dead_first_year")] = 
  lapply(df.import[,names(df.import) %in% c("sex", "copd_lung", "cancer", "liver_disease", "cardiovascular", "access_type", "bmi", "resulting_autonomy","diabetes_with_compl","peripheral_artery_disease","dementia","dead_first_year")], as.factor )
df.import = df.import[complete.cases(df.import),]
df.import$resulting_autonomy = relevel(df.import$resulting_autonomy, ref = "normal")
df.import$bmi = relevel(df.import$bmi, ref = "normal/high")

#define outcome event
df.import$outcome[df.import$dead_first_year == 0] = "Alive"
df.import$outcome[df.import$dead_first_year == 1] = "Dead"
df.import[,"outcome"] = as.factor(df.import[,"outcome"])
df.import$dead_first_year = NULL

#split data
set.seed(30)
indizesTrain = createDataPartition(df.import$outcome, p = 0.8, list = FALSE)
train_data = df.import[indizesTrain,]
test_data = df.import[-indizesTrain,]

variableImportance <- randomForest(outcome ~ ., data=train_data, ntree=500, keep.forest=FALSE, importance=TRUE)
varImpPlot(variableImportance, sort = TRUE)
#nrow(subset(train_data, access_type=="fistula/graft"& bmi == "normal/high"))

#model
#logit.out = glm(dead_first_year ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi, data = train_data, family= binomial(logit))
#summary(logit.out)

#with caret (incl. internal validation with 10-fold-cross-validation)
tc = trainControl("cv",10, classProbs = TRUE, summaryFunction = twoClassSummary)
logit.out = train(outcome ~ age_10*cancer+copd_lung+liver_disease+access_type+cardiovascular+bmi+resulting_autonomy+albumin+cci+diabetes_with_compl+peripheral_artery_disease, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
logit_interact.out = train(outcome ~ age_10*cancer+sex+copd_lung+liver_disease+access_type*cardiovascular+access_type*bmi+resulting_autonomy+albumin+cci, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
logit_all.out = train(outcome ~ ., data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")

summary(logit_interact.out)
logit_interact.out

#logit_glm = glm(outcome ~ age_10*cancer+sex+copd_lung+liver_disease+access_type*cardiovascular+access_type*bmi+resulting_autonomy, data=train_data, family=binomial(logit))
#back = step(logit_glm)  
#rf.out = train(outcome ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi+resulting_autonomy, data=train_data, method="rf",trControl=tc, metric = "ROC")
rf_all.out = train(outcome ~ ., data=train_data, method="rf",trControl=tc, metric = "ROC")
#svm.out = train(outcome ~ ., data=train_data, method="svmLinear",trControl=tc, metric = "ROC")

curClassifier = rf_all.out
#external validation with test set
predict.out = predict(curClassifier, test_data, type = "prob")
myroc = pROC::roc(test_data$outcome, as.vector(predict.out[,2]))
plot(myroc, print.thres = "best")
currentScore = auc(myroc) 

##adjust optimal cut-off threshold for class probabilities
threshold = coords(myroc,x="best",best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
predCut = factor(ifelse(predict.out$Dead > threshold, "Dead", "Alive") )
##Confusion Matrix 
curConfusionMatrix = confusionMatrix(predCut, test_data$outcome, positive = "Dead")
curConfusionMatrix
curConfusionMatrix$overall[3]

