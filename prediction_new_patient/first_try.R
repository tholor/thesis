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
library(randomForest)
library(RANN)
library(e1071)

con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT * FROM dbo.02_predictors_monthly pred
LEFT JOIN 01_outcomes_monthly_test outc
ON pred.fk_patient = outc.fk_patient AND pred.month=outc.month AND pred.year = outc.year
where elapsed_time_on_dialysis = 0")
df.import = dbGetQuery(con, strQuery)

#preprocess
raw_data = df.import[,!names(df.import) %in% c("fk_patient", "month","year","fdod","hcO3_bicarb","elapsed_time_on_dialysis","num_hd_sessions","planned_duration")]
raw_data$death_event = ifelse(raw_data$dead_next_month+raw_data$dead_this_month > 0,1,0)
raw_data = raw_data[,!names(raw_data) %in% c("dead_next_month","dead_this_month","dod")]

raw_data$death_event[raw_data$death_event == 0] = "No"
raw_data$death_event[raw_data$death_event == 1] = "Yes"
raw_data[,"death_event"] = as.factor(raw_data[,"death_event"])

#PCA
pca_model = preProcess(raw_data[,!names(raw_data) %in% c("death_event")], method = "pca")
pca_data = predict.preProcess(pca_model, raw_data[,!names(raw_data) %in% c("death_event")])

raw_data$death_event = as.factor(raw_data$death_event)
imputation_model = preProcess(raw_data[,!names(raw_data) %in% c("death_event")], method = "knnImpute")
imputed_data = predict.preProcess(imputation_model, raw_data[,!names(raw_data) %in% c("death_event")])
imputed_data$death_event = raw_data$death_event
princomp(raw_data[,!names(imputed_data) %in% c("death_event")])
str(raw_data)
#### SPLIT THE DATA ####
##Split to 70% Train Data, 20% Test Data, 10% Final Validation Data
set.seed(21)
indizesTrain = createDataPartition(imputed_data$death_event, p = 0.7, list = FALSE)
train_data = imputed_data[indizesTrain,]
test_data = imputed_data[-indizesTrain,]

variableImportance <- randomForest(death_event ~ ., data=train_data, ntree=500, keep.forest=FALSE, importance=TRUE)
varImpPlot(variableImportance, sort = TRUE)
#### TRAIN THE MODEL ####
cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
#set grid with different training paarameters
newGrid = expand.grid(mtry = c(15))
#classifierRpart =           train(case_success ~ ., data = train_data, trControl = cvCtrl, metric = "ROC", method = "rpart")
#classifierGBM = train(case_success ~ ., data = train_data, trControl = cvCtrl, metric = "ROC", method = "gbm")
#classifierRandomForest =    train(case_success ~ ., data = train_data, trControl = cvCtrl, metric = "Accuracy", method = "rf", tuneGrid = newGrid)
set.seed(21)
classifierRandomForest = train(death_event ~ ., data = train_data, trControl = cvCtrl, method = "rf", metric="ROC", tuneGrid = newGrid)
curClassifier = classifierRandomForest
#beep()
#curClassifier = classifierRpart

predRoc = predict(curClassifier, test_data, type = "prob")
myroc = pROC::roc(test_data$death_event, as.vector(predRoc[,2]))
plot(myroc, print.thres = "best")
currentScore = auc(myroc) #that's our "final" evaluation score 
currentScore

##adjust optimal cut-off threshold for class probabilities
#predAdj = predict(curClassifier, testData, type = "prob")
threshold = coords(myroc,x="best",best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
predCut = factor( ifelse(predRoc[, "Yes"] > threshold, "Yes", "No") )
#predCut = relevel(predCut, "yes")   #try that, if error occurs

##Confusion Matrix (Accuracy, Spec, Sens etc.)
curConfusionMatrix = confusionMatrix(predCut, test_data$death_event, positive = "Yes")
curConfusionMatrix
curConfusionMatrix$overall[3]
