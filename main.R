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

#import & convert all tables from MYSQL
source("import_all_tables.R")
load("df_hemo")

 summary_patients = basic_summary(df.patients)
 summary_hemo = basic_summary(df.hemo)
 summary_icds= basic_summary(df.icds)
 summary_lab_types= basic_summary(df.lab_types)
 summary_vasc_accesses= basic_summary(df.vasc_accesses)
 summary_allergies= basic_summary(df.allergies)
 summary_complications= basic_summary(df.complications)
 summary_diagnoses= basic_summary(df.diagnoses)
 summary_hospitalizations= basic_summary(df.hospitalizations)
 summary_labs= basic_summary(df.labs)
 summary_noshows= basic_summary(df.noshows)
 summary_procedures= basic_summary(df.procedures)
 summary_signsyms= basic_summary(df.signsyms)
 summary_vitalsigns= basic_summary(df.vitalsigns)

#Write Summaries to Excel 
write_all_summaries()

#close all connections
all_cons <- dbListConnections(MySQL())
for(con in all_cons){
  dbDisconnect(con)}

##########################
# some ad hoc basic analyses
# 1. HISTOGRAM / BOXPLOT
#hemo
#boxplot
ggplot(df.hemo, aes(y = dialysate_temp, x = factor(1))) + geom_boxplot()
ggplot(subset(df.hemo,dialysate_temp < 100), aes(y = dialysate_temp, x = factor(1))) + geom_boxplot()
ggplot(df.hemo, aes(y = avg_bp_systolic, x = factor(1))) + geom_boxplot()
ggplot(subset(df.hemo,avg_bp_systolic < 500), aes(y = avg_bp_systolic, x = factor(1))) + geom_boxplot()

#histo
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "histogram", binwidth = 200)
qplot(dialysate_flow, data = subset(df.hemo,dialysate_flow > 10000), geom = "histogram", binwidth = 100000)
ggplot(df.hemo, aes(dialysate_name))+ geom_histogram() + facet_grid(~fk_location) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "histogram", binwidth = 200)
#density
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "density")
qplot(dialysate_flow, data = subset(df.hemo,dialysate_flow < 10000), geom = "density")
#scatter
qplot(x=heparin, y= dialysate_flow, data = df.hemo, geom = "point")
qplot(x=dialysate_temp, y= dialysate_flow, data = df.hemo, geom = "point")

# 2. OUTLIER DETECTION
glimpse(tbl.hemo)
  #simple & fast (but not 100% reliable)
  variable_vector = df.hemo$dialysate_temp
 out =  getOutliers(variable_vector, method ="I")
 df.hepa_clean = variable_vector[-out$iRight]

# 3. TABLEPLOTS
#removing character columns (cannot be handled by tableplot)
#patients
df = df.patients
cols_text= sapply(df, is.character)
df = df[,!cols_text]
df$dob= datetime2fac(df$dob, rng = range(df$dob, na.rm = TRUE))
df$dod= datetime2fac(df$dod, rng = range(df$dod, na.rm = TRUE))
df_prep = tablePrepare(df)
tableplot(df_prep, sortCol = dob)
#hemo
df = df.hemo
cols_text= sapply(df, is.character)
df = df[,c("heparin","date","duration","dialysate_flow","blood_flow","fluid_removed","avg_arterial_pressure",
           "avg_bp_systolic","avg_bp_diastolic","dialysate_name")]
df$date= datetime2fac(df$date, rng = range(df$date, na.rm = TRUE))
df_prep = tablePrepare(df)
tableplot(df_prep, sortCol = duration)
