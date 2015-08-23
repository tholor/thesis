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
library(tidyr)

#import data
#currently missing: CKD reason
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT ROUND((age/10),0) as age_10, sex, copd_lung, cancer, GREATEST(mild_liver_disease, severe_liver_disease) as liver_disease, cardiovascular,
                  (CASE access_at_begin WHEN 'fistula_usable' THEN 'fistula/graft' WHEN 'graft_usable' THEN 'fistula/graft' ELSE 'catheter' END) as access_type,
                  (CASE WHEN bmi < 20 THEN 'low' ELSE 'normal/high' END) AS bmi, resulting_autonomy,
                  (CASE WHEN primary_renal_disease = 'hypertension' OR primary_renal_disease = 'other' THEN 'standard' ELSE primary_renal_disease END) as primary_renal_disease, 
                  dead_first_year FROM 03_model_begin_to_1year")
df.import = dbGetQuery(con, strQuery)
#close DB-Connection
dbDisconnect(con)

#preprocess data
str(df.import)
df.import[, names(df.import) %in% c("sex", "copd_lung", "cancer", "liver_disease", "cardiovascular", "access_type", "bmi","resulting_autonomy","primary_renal_disease", "dead_first_year")] = 
  lapply(df.import[,names(df.import) %in% c("sex", "copd_lung", "cancer", "liver_disease", "cardiovascular", "access_type", "bmi", "resulting_autonomy","primary_renal_disease","dead_first_year")], as.factor )
df.import = df.import[complete.cases(df.import),]
df.import$resulting_autonomy = relevel(df.import$resulting_autonomy, ref = "normal")
df.import$bmi = relevel(df.import$bmi, ref = "normal/high")
df.import$primary_renal_disease = relevel(df.import$primary_renal_disease, ref = "standard")

#define outcome event
df.import$outcome[df.import$dead_first_year == 0] = "Alive"
df.import$outcome[df.import$dead_first_year == 1] = "Dead"
df.import[,"outcome"] = as.factor(df.import[,"outcome"])
df.import$dead_first_year = NULL

#split data
#set.seed(10)
#indizesTrain = createDataPartition(df.import$outcome, p = 0.8, list = FALSE)
#train_data = df.import[indizesTrain,]
#test_data = df.import[-indizesTrain,]
train_data = df.import

#nrow(subset(train_data, access_type=="fistula/graft"& bmi == "normal/high"))

#model
#logit.out = glm(dead_first_year ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi, data = train_data, family= binomial(logit))
#summary(logit.out)

#with caret (incl. internal validation with 10-fold-cross-validation)
  tc = trainControl("repeatedcv",number=5,repeats=10, classProbs = TRUE, summaryFunction = twoClassSummary)
  #logit.out = train(outcome ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
  logit_interact.out = train(outcome ~ age_10*cancer+sex+copd_lung+liver_disease+access_type*cardiovascular+primary_renal_disease+access_type*bmi+resulting_autonomy, data=train_data, method="glm",trControl=tc,family=binomial(logit), metric = "ROC")
  summary(logit_interact.out)
  logit_interact.out
#logit_glm = glm(outcome ~ age_10*cancer+sex+copd_lung+liver_disease+access_type*cardiovascular+access_type*bmi+resulting_autonomy, data=train_data, family=binomial(logit))
  #back = step(logit_glm)  
#rf.out = train(outcome ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi+resulting_autonomy, data=train_data, method="rf",trControl=tc, metric = "ROC")
  #rf_all.out = train(outcome ~ ., data=train_data, method="rf",trControl=tc, metric = "ROC")
  #svm.out = train(outcome ~ age_10+sex+copd_lung+cancer+liver_disease+cardiovascular+access_type+bmi+resulting_autonomy, data=train_data, method="svmLinear",trControl=tc, metric = "ROC")

curClassifier = logit_interact.out
#external validation with test set
predict.out = predict(curClassifier, test_data, type = "prob")
myroc = pROC::roc(test_data$outcome, as.vector(predict.out[,2]))
plot(myroc, print.thres = "best")
currentScore = auc(myroc) 

##adjust optimal cut-off threshold for class probabilities
threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
predCut = factor( ifelse(predict.out$Dead > threshold, "Dead", "Alive") )
##Confusion Matrix 
curConfusionMatrix = confusionMatrix(predCut, test_data$outcome, positive = "Dead")
curConfusionMatrix
curConfusionMatrix$overall[3]

#____________________________________
#descriptive stats of the cohort
#variable counts
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT age, sex, copd_lung, cancer, GREATEST(mild_liver_disease, severe_liver_disease) as liver_disease, cardiovascular,
                  (CASE access_at_begin WHEN 'fistula_usable' THEN 'fistula/graft' WHEN 'graft_usable' THEN 'fistula/graft' ELSE 'catheter' END) as access_type,
                   (CASE WHEN bmi < 20 THEN 'low' ELSE 'normal/high' END) AS bmi, resulting_autonomy, primary_renal_disease,
                  arthropathies, peripheral_artery_disease, cerebrovascular, diabetes_no_compl,
                  dead_first_year 
                  FROM 03_model_begin_to_1year
                  WHERE age is not null 
                  AND sex is not null
                  AND copd_lung is not null
                  AND cancer is not null
                  AND cardiovascular is not null
                  AND resulting_autonomy is not null
                  HAVING liver_disease is not null
                  AND access_type is not null")
df.import = dbGetQuery(con, strQuery)
dbDisconnect(con)
df.import$age_10 = round(df.import$age/10,0)
#df.import$bmi_low = ifelse(df.import$bmi<20,"low","normal/high")
# df.import = df.import[complete.cases(df.import),]

test = gather(df.import, variable, value, -age) %>%
  count(variable,value)

#survival times
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT pid,dod,months_on_dial_bef_dead,dod,adj_fdod, datediff(dod,adj_fdod) as days_survived FROM prep_outcomes where months_on_dial_bef_dead >= 0")
df.outcome = dbGetQuery(con, strQuery)
df.survival = merge(df.import, df.outcome, by="pid")
dbDisconnect(con)

df.survival_part = dplyr::select(df.survival, months_on_dial_bef_dead, days_survived)
df.survival_part$'14_days' = ifelse(df.survival_part$days_survived <= 14,1,0)
df.survival_part$'30_days' = ifelse(df.survival_part$days_survived <= 30,1,0)
df.survival_part$'3_months' = ifelse(df.survival_part$months_on_dial_bef_dead <= 3,1,0)
df.survival_part$'6_months' = ifelse(df.survival_part$months_on_dial_bef_dead <= 6,1,0)
df.survival_part$'12_months' = ifelse(df.survival_part$months_on_dial_bef_dead <= 12,1,0)
df.survival_part$'3_years' = ifelse(df.survival_part$months_on_dial_bef_dead <= 36,1,0)

sum(df.survival_part$'14_days',na.rm=TRUE)
sum(df.survival_part$'30_days',na.rm=TRUE)
sum(df.survival_part$'3_months',na.rm=TRUE)
sum(df.survival_part$'6_months',na.rm=TRUE)
sum(df.survival_part$'12_months',na.rm=TRUE)
sum(df.survival_part$'3_years',na.rm=TRUE)


