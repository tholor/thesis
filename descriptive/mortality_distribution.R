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
source("basic_descriptive.R")
## Establish MySQL Connection
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

#_____________________________________
####### RELATION HGB AND EPO ########
#_____________________________________
strQuery = paste0("SELECT pid,dod,adj_fdod FROM dbo.dim_patients2 WHERE dod is not null AND adj_fdod is not null")
df.table = dbGetQuery(con, strQuery)
df.table$adj_fdod = as.Date(df.table$adj_fdod)
df.table$dod = as.Date(df.table$dod)
df.table$survival_months = as.numeric((df.table$dod - df.table$adj_fdod)/30.43)

#plotting the number of deaths per month on dialysis (using only patients where dod and adj_fdod is available!)
qplot(survival_months, data = df.table, geom = "histogram", binwidth = 1)



