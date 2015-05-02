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
#library(lubridate)
library(ggplot2)
source("helper.R")
source("import_data.R")
source("basic_descriptive.R")
## Establish MySQL Connection
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

 df.patients = converted_import("dim_patients")
 df.icds= converted_import('dim_icds')
 df.lab_types= converted_import('dim_lab_types')
 df.vasc_accesses= converted_import('dim_vasc_accesses')
 df.allergies= converted_import('fact_allergies')
 df.complications= converted_import('fact_complications')
 df.diagnoses= converted_import('fact_diagnoses')
 df.hospitalizations= converted_import('fact_hospitalizations')
 df.labs= converted_import('fact_labs')
 df.noshows= converted_import('fact_noshows')
 df.procedures= converted_import('fact_procedures')
 df.signsyms= converted_import('fact_signsyms')
 df.vitalsigns= converted_import('fact_vitalsigns')
 df.hemo = converted_import("dim_hemodialyses")

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
write_xlsx(summary_patients, "basic_R_summaries.xlsx", "patients")
write_xlsx(summary_hemo, "basic_R_summaries.xlsx", "hemo")
write_xlsx(summary_icds, "basic_R_summaries.xlsx", "icds")
write_xlsx(summary_lab_types, "basic_R_summaries.xlsx", "lab_types")
write_xlsx(summary_vasc_accesses, "basic_R_summaries.xlsx", "vasc_accesses")
write_xlsx(summary_allergies, "basic_R_summaries.xlsx", "allergies")
write_xlsx(summary_complications, "basic_R_summaries.xlsx", "complications")
write_xlsx(summary_diagnoses, "basic_R_summaries.xlsx", "diagnoses")
write_xlsx(summary_hospitalizations, "basic_R_summaries.xlsx", "hospitalizations")
write_xlsx(summary_labs, "basic_R_summaries.xlsx", "labs")
write_xlsx(summary_noshows, "basic_R_summaries.xlsx", "noshows")
write_xlsx(summary_procedures, "basic_R_summaries.xlsx", "procedures")
write_xlsx(summary_signsyms, "basic_R_summaries.xlsx", "signsyms")
write_xlsx(summary_vitalsigns, "basic_R_summaries.xlsx", "vitalsigns")

#close all connections
all_cons <- dbListConnections(MySQL())
for(con in all_cons){
  dbDisconnect(con)}

##########################
# some more basic analyses



# 1. histogram / density
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "histogram", binwidth = 200)
qplot(dialysate_flow, data = subset(df.hemo,dialysate_flow < 100000), geom = "histogram", binwidth = 10000)
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "histogram", binwidth = 200)
qplot(heparin, data = subset(df.hemo,heparin < 10000), geom = "histogram", binwidth = 200)



hist.data.frame(df.hemo)
hist(df.hemo$heparin)
# 2. outlier detection
tbl.hemo = tbl_df(df.hemo)
glimpse(tbl.hemo)
sub_hemo = df.hemo[sample(1:nrow(df.hemo),100000,replace=FALSE),]
qplot(heparin, data = sub_hemo, geom = "density")
uni.plot(sub_hemo[,c(heparin,)) 
# hemo
tbl.hemo %>% 
  ggplot(aes(x=heparin)) + 
  geom_point(alpha=0.5) +
 # facet_grid(~ shift) + 
  #stat_smooth(method = lm, formula = y ~ poly(x,2)) + 
  theme_bw()

# 3. tableplots
#removing character columns (cannot be handled by tableplot)
df = df.patients
cols_text= sapply(df, is.character)
df = df[,!cols_text]
df$dob= datetime2fac(df$dob, rng = range(df$dob, na.rm = TRUE))
df$dod= datetime2fac(df$dod, rng = range(df$dod, na.rm = TRUE))
df_prep = tablePrepare(df)
tableplot(df_prep, sortCol = dob)

df = df.hemo
cols_text= sapply(df, is.character)
df = df[,c("heparin","date","duration","dialysate_flow","blood_flow","fluid_removed","avg_arterial_pressure",
           "avg_bp_systolic","avg_bp_diastolic","dialysate_name")]
df$date= datetime2fac(df$date, rng = range(df$date, na.rm = TRUE))
df_prep = tablePrepare(df)
tableplot(df_prep, sortCol = date)
