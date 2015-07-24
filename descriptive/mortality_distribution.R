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
library(survival)
library(data.table)
#library(extremevalues)
#library(lubridate)
library(ggplot2)
library(epiR)
source("helper.R")
source("import_data.R")
source("descriptive\\basic_descriptive.R")
## Establish MySQL Connection
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

#distribution of deaths
strQuery = paste0("SELECT pid,dod,adj_fdod FROM dbo.dim_patients2 WHERE dod is not null AND adj_fdod is not null")
df.table = dbGetQuery(con, strQuery)
df.table$adj_fdod = as.Date(df.table$adj_fdod)
df.table$dod = as.Date(df.table$dod)
df.table$survival_months = as.numeric((df.table$dod - df.table$adj_fdod)/30.43)

#plotting the number of deaths per month on dialysis (using only patients where dod and adj_fdod is available!)
qplot(survival_months, data = df.table, geom = "histogram", binwidth = 1, xlab = "months on dialysis", ylab = "number of deaths", xlim=c(0,60))


#survival analysis
strQuery = paste0("SELECT pid,dod,adj_fdod, last_dialysis_lib FROM dbo.dim_patients2 WHERE adj_fdod is not null AND (last_dialysis_lib is not null OR dod is not null)")
patients = dbGetQuery(con, strQuery)

patients$dod = as.Date(patients$dod)
patients$fdod = as.Date(patients$adj_fdod)
patients$ldod = as.Date(patients$last_dialysis_lib)
patients$dead = as.factor(ifelse(is.na(patients$dod),0,1))
x = as.Date(ifelse(is.na(patients$dod),patients$ldod,patients$dod),origin="1970-01-01") #end date of dialysis
patients$survival_months = round((x-patients$fdod)/30.43)


#Survival Time
surv_data = data.frame(time = as.numeric(patients[,"survival_months"]),
                       event = as.numeric(patients$dead)-1)
surv_data = subset(surv_data, !is.na(time))
surv_data = subset(surv_data, time < 200)

str(surv_data)

#survival curve
kmsurvival <- survfit(Surv(time,event)~1, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", xlim = c(0,60), ylab="Survival Probability",col=c("blue"))
title("KM survival")

