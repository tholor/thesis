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
library(pROC)

con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT * FROM 03_model_begin_to_1year WHERE (albumin is not null OR calcium is not null)")
df.import = dbGetQuery(con, strQuery)
strQuery = paste0("SELECT pid, dod, last_dialysis_lib FROM dim_patients2")
df.patient = dbGetQuery(con, strQuery)
df.merged = merge(df.import, df.patient, by = "pid")

#preprocess
data = df.merged[,!names(df.merged) %in% c("sex","pid", "ethnic","year","max_date_diff","elapsed_time_on_dialysis","num_hd_sessions","planned_duration", "dead_first_year")]
#raw_data$outcome = ifelse(raw_data$dead_next_month+raw_data$dead_this_month > 0,1,0)
#raw_data = raw_data[,!names(raw_data) %in% c("dead_next_month","dead_this_month","dod")]
data$access_at_begin = as.factor(data$access_at_begin)
data$dod = as.Date(data$dod)
data$adj_fdod = as.Date(data$adj_fdod)
data$ldod = as.Date(data$last_dialysis_lib)
data$dead = as.factor(ifelse(is.na(data$dod),0,1))
x = as.Date(ifelse(is.na(data$dod),data$ldod,data$dod),origin="1970-01-01") #end date of dialysis
data$diatime = round((x-data$adj_fdod)/30)

#imputation to deal with missing data



#fit cox PH model
time = as.numeric(data$diatime)
event = as.numeric(data$dead)
surv_data = data[,!names(data) %in% c("adj_fdod","dod","last_dialysis_lib","ldod","diatime","dead")]
coxph <- coxph(Surv(time,event) ~ ., data = surv_data, method="breslow", model = TRUE)
summary(coxph)

basehaz(coxph)
plot(survfit(coxph))
