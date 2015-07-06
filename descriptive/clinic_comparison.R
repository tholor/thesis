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

## Establish MySQL Connection
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
#_____________________________________
####### Get patients and clinic ########
#_____________________________________
strQuery = paste0("Select pat.pid,loc.location,loc.name as loc_name, cast(pat.dob AS DATE) as dob, pat.adj_fdod, ethnic FROM dbo.dim_patients2 pat 
INNER JOIN miqs.treatment_history treat 
ON pat.pid = treat.pid
INNER JOIN dbo.dim_locations loc 
ON treat.location = loc.location
WHERE loc.name LIKE 'Liberty Dialysis%'
GROUP by pid,location")
patients = dbGetQuery(con, strQuery)

patients$dob = as.Date(patients$dob)
#patients$dod = as.Date(patients$dod)
patients$fdod = as.Date(patients$adj_fdod)
#patients$high_iron = as.numeric(patients$higher_416mg_monthly)
patients$ldod = as.Date(patients$last_dialysis_lib)
patients$dead = as.factor(ifelse(is.na(patients$dod),0,1))
patients$ageAtDia = (patients$fdod - patients$dob)/365

summary <- patients %>%
  group_by(loc_name) %>%
    summarise(obs = n(),
              avg_age = mean(ageAtDia, na.rm = TRUE))

ethnics <- patients %>%
  group_by(loc_name, ethnic) %>%
  summarise(obs = n())

temp = merge(summary, ethnics, by="loc_name")
