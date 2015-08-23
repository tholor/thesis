rm(list = ls())
gc()
setwd("C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R")
source("initialize_libraries.R")
source("models\\helper_functions.R")

#_______________________
#SETTINGS
#_______________________
data_set = "" 
featureSelection = "rfe" #one of filter, rfse, GA, SA
discretizeLabs = "False" #True/False
method= "log" #rf, nb, log
exclude_columns = c("phosphorus","bmi","arterial_press_increased","venous_press_increased","ktv","connective_tissue","pain_chest","pain_elsewhere","pain_leg","eGFR","hemiplegia","lymphoma","leukemia","down_syndrome","hiv")
comment = "with bmi-> mauri"
outcome_type = "1 year"
#_______________________
#Import
#_______________________
df.import = query_data("full")
strQuery = paste0("SELECT pid, dod, last_dialysis_lib FROM dim_patients2")
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
df.patient = dbGetQuery(con, strQuery)
dbDisconnect(con)
df.merged = merge(df.import, df.patient, by = "pid")
#_______________________
#Preprocess
#_______________________
df.preProcess = convert_data_types(df.merged, outcome_type) 
df.preProcess = df.preProcess[,!(names(df.preProcess) %in% exclude_columns)]
df.preProcess = df.preProcess[complete.cases(df.preProcess[,!names(df.preProcess) %in% c("dod")]),]

#preprocess
#data = df.merged[,!names(df.merged) %in% c("sex","pid","year","max_date_diff","elapsed_time_on_dialysis","num_hd_sessions","planned_duration", "dead_first_year")]
#raw_data$outcome = ifelse(raw_data$dead_next_month+raw_data$dead_this_month > 0,1,0)
#raw_data = raw_data[,!names(raw_data) %in% c("dead_next_month","dead_this_month","dod")]
data = df.preProcess
data$dod = as.Date(data$dod)
data$adj_fdod = as.Date(data$adj_fdod)
data$ldod = as.Date(data$last_dialysis_lib)
data$dead = as.factor(ifelse(is.na(data$dod),0,1))
x = as.Date(ifelse(is.na(data$dod),data$ldod,data$dod),origin="1970-01-01") #end date of dialysis
data$diatime = round((x-data$adj_fdod)/30)


#REPEATED CV

#fit cox PH model
time = as.numeric(data$diatime)
event = as.numeric(data$dead)
surv_data = data[,!names(data) %in% c("adj_fdod","dod","start_year","outcome","last_dialysis_lib","ldod","diatime","dead")]
coxph <- coxph(Surv(time,event) ~ ethnic + cardiovascular + hypertension + dementia + 
                 ulcer + primary_renal_disease + albumin + potassium + creatinine + 
                 epogen_usage + access_flow_poor + resulting_autonomy + age_10 + 
                 liver_disease + pain, data = surv_data, method="breslow", model = TRUE)

coxph_step <- coxph(Surv(time,event) ~ ethnic + cardiovascular + 
                      hypertension + ulcer + cerebrovascular + albumin + calcium + 
                      potassium + creatinine + ferritin + resulting_autonomy + 
                      age + liver_disease + access_type, data = surv_data, method="breslow", model = TRUE)
step(coxph_step)
#c index
coxph$concordance[1]/(coxph$concordance[2]+coxph$concordance[1])

coxph2 = cph(Surv(time,event) ~ ethnic + cardiovascular + hypertension + dementia + 
               ulcer + primary_renal_disease + albumin + potassium + creatinine + 
               epogen_usage + access_flow_poor + resulting_autonomy + age + 
               liver_disease + pain,data=surv_data,x=T,y=T,surv=TRUE)
validate(coxph2)

summary(coxph)
basehaz(coxph)
plot(survfit(coxph2))
#KM
kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red","green"))
#predictions for t = 1 year

#function for prediction
risk = function(model, newdata, time) {
as.numeric(1-summary(survfit(model, newdata = newdata, se.fit = F, conf.int = F), times = time)$surv)
}
as.numeric(1-summary(survfit(coxph, newdata = surv_data, se.fit = F, conf.int = F), times = 12)$surv)

#get predcitions
data$prediction = risk(coxph, surv_data,12)
#evaluation
myroc = pROC::roc(data$outcome, data$prediction)
plot(myroc, print.thres = "best")
currentScore = auc(myroc) 
threshold = coords(myroc,x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
predCut = factor( ifelse(data$prediction > threshold, "Dead", "Alive") )
##Confusion Matrix 
curConfusionMatrix = confusionMatrix(predCut, data$outcome, positive = "Dead")
curConfusionMatrix
curConfusionMatrix$overall[3]

