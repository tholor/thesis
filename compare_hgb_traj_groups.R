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
library(extremevalues)
#library(lubridate)
library(ggplot2)
source("helper.R")
source("import_data.R")
source("basic_descriptive.R")
## Establish MySQL Connection
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

strQuery = paste0("SELECT * FROM dbo.res_hgb_traj res LEFT JOIN dbo.prep_outcomes outc ON res.fk_patient = outc.pid")
df.table = dbGetQuery(con, strQuery)
df.table$first_dialysis = as.POSIXct(df.table$first_dialysis)
df.group1 = df.table[which(df.table$GROUP ==1),]
df.group2 = df.table[which(df.table$GROUP ==2),]
mean(df.group1$first_dialysis, na.rm=TRUE)
mean(df.group2$first_dialysis, na.rm=TRUE)

ggplot(df.table, aes(y = first_dialysis, x = factor(GROUP))) + geom_boxplot()

#p-value = 4.135e-08
t.test(df.table[which(df.table$GROUP ==1 & !is.na(df.table$age)),]$age,
       df.table[which(df.table$GROUP ==2 & !is.na(df.table$age)),]$age)

#p-value = 3.677e-05
t.test(df.table[which(df.table$GROUP ==1 ),]$num_heart_attacks,
       df.table[which(df.table$GROUP ==2 ),]$num_heart_attacks)

# p-value = 0.4367
t.test(df.table[which(df.table$GROUP ==1 & !is.na(df.table$num_hospitalizations)),]$num_hospitalizations,
       df.table[which(df.table$GROUP ==2 & !is.na(df.table$num_hospitalizations)),]$num_hospitalizations)
