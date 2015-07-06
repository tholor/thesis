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
strQuery = paste0("SELECT * FROM dbo.prep_hgb_and_epo")
df.table = dbGetQuery(con, strQuery)

### correlation and distribution of hemoglobin and epogen
#remove outlier
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - 4*H)] <- NA
  y[x > (qnt[2] + 4*H)] <- NA
  return(y)
}

df.table$monthly_hgb_value_clean = remove_outliers(df.table$monthly_hgb_value)

#plots
#hemoglobin
qplot(monthly_hgb_value_clean, data = df.table, geom = "density")
qplot(monthly_hgb_value_clean, data = df.table, geom = "histogram", binwidth = 0.1)

#epo
qplot(avg_epo_dose, data = df.table, geom = "density")
qplot(avg_epo_dose_per_kg, data = df.table, geom = "density")

qplot(avg_epo_dose, data = df.table, geom = "histogram", binwidth = 1000)
#both
qplot(x=avg_epo_dose, y= monthly_hgb_value_clean, data = df.table, geom = "point")
qplot(x=avg_epo_dose_per_kg, y= monthly_hgb_value_clean, data = df.table, geom = "point")

#correlation
cor(df.table$monthly_hgb_value_clean, df.table$avg_epo_dose, use="complete.obs")
cor.test(df.table$monthly_hgb_value_clean, df.table$avg_epo_dose, alternative="two.sided")
#per kg
cor(df.table$monthly_hgb_value_clean, df.table$avg_epo_dose_per_kg, use="complete.obs")
cor.test(df.table$monthly_hgb_value_clean, df.table$avg_epo_dose_per_kg, alternative="two.sided")


#______________________________________
#______________________________________________#
#######survival analysis with groups from SQL (BY RANGE OF HGB) ###
strQuery = paste0("SELECT * FROM
                   prep_hgb_groups_by_range_and_iron
                  WHERE epogen_flag = 'TRUE' and num_epo_taken > 20")
groups = dbGetQuery(con, strQuery)
names(groups)[names(groups) == 'fk_patient'] <- 'pid'
#get patients
strQuery = paste0("SELECT * FROM dbo.dim_patients2 WHERE adj_fdod > '2006-01-01'")
patients= dbGetQuery(con, strQuery)
#merge
patients = merge(groups, patients, by= 'pid')

#extract necessary fields
patients = patients[,c("pid","dob","dod","active_status","first_dialysis_lib","last_dialysis_lib","adj_fdod","group")]
patients$GROUP = as.factor(patients$group)
patients$dob = as.Date(patients$dob)
patients$dod = as.Date(patients$dod)
patients$fdod = as.Date(patients$adj_fdod)
#patients$high_iron = as.numeric(patients$higher_416mg_monthly)
patients$ldod = as.Date(patients$last_dialysis_lib)
patients$dead = as.factor(ifelse(is.na(patients$dod),0,1))
patients$ageAtDia = (patients$fdod - patients$dob)/365
#groups$dead = patients$dead

x = as.Date(ifelse(is.na(patients$dod),patients$ldod,patients$dod),origin="1970-01-01") #end date of dialysis
#x = as.Date(ifelse(is.na(patients$dod),as.Date("2014-02-01"),patients$dod),origin="1970-01-01") #end date of dialysis
patients$diatime = round((x-patients$fdod)/30)
#test = patients[patients$group!=4,]
#percentage of dead ppl in each group
#ggplot(data=patients,mapping = aes(x=GROUP, fill=dead)) + geom_bar(position="dodge")
#a =data.table(ddply(patients,c("GROUP","dead"),summarize,count=length(pid)))
#b = a[,list(dead,count,percent=(count/sum(count))*100),by=c("GROUP")]

#cat("Dead % in group")
#print(b)
#ggplot(data=b,mapping=aes(x=GROUP,y=percent,fill=dead)) +geom_bar(position="dodge",stat="identity")

#test: 
patients = subset(patients, diatime > 12)

#Survival Time
surv_data = data.frame(time = as.numeric(patients[,"diatime"]),
                       event = as.numeric(patients$dead)-1,
                       group = patients[,"GROUP"])
#time = as.numeric(patients[,"diatime"])
#event = as.numeric(patients[,"dead"])-1
#group = patients[,"GROUP"]

#all groups
kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red","green","grey","yellow","black","pink"))
title("KM survival")
legend("topright", inset=.05, title="Group",
       levels(surv_data$group), fill = c("blue","red","green","grey","yellow","black","pink"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data)

# groups 1-4 (iron)
surv_data2= subset(surv_data, group %in% c("10/11-highIron","10/11-lowIron","11/12-highIron","11/12-lowIron"))
kmsurvival2 <- survfit(Surv(time,event) ~ group, data = surv_data2)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time (months)", ylab="Survival Probability",col=c("darkgreen","green","orange", "red"))
title("KM survival")
legend("bottomleft", inset=.05, title="Group",
       c("10/11-highIron","10/11-lowIron","11/12-highIron","11/12-lowIron"), fill = c("darkgreen","green","orange", "red"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data2)

# groups 1-4 (epo)
surv_data2= subset(surv_data, group %in% c("10/11-highEpo","10/11-lowEpo","11/12-highEpo","11/12-lowEpo"))
kmsurvival2 <- survfit(Surv(time,event) ~ group, data = surv_data2)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time (months)", ylab="Survival Probability",col=c("darkgreen","green","orange", "red"))
title("KM survival")
legend("bottomleft", inset=.05, title="Group",
       c("10/11-highEpo","10/11-lowEpo","11/12-highEpo","11/12-lowEpo"), fill = c("darkgreen","green","orange", "red"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data2)

# groups 1-3
surv_data2= subset(surv_data, group != 4 )
kmsurvival2 <- survfit(Surv(time,event) ~ group, data = surv_data2)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red","green"))
title("KM survival")
legend("topright", inset=.05, title="Group",
       c("10-11","11-12","12-13"), fill = c("blue","red","green"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data2)

# groups 1-2
surv_data3= subset(surv_data, group %in% c(1,2) )
kmsurvival3 <- survfit(Surv(time,event) ~ group, data = surv_data3)
summary(kmsurvival3)
plot(kmsurvival3, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red"))
title("KM survival")
legend("topright", inset=.05, title="Group",
       c("10-11","11-12"), fill = c("blue","red"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data3)

epi.insthaz(kmsurvival3, conf.level = 0.95)

#____________________________________
#DESCRIPTIVE SUMMARY of the two group
patients_desc = patients
strQuery = paste0("SELECT pid, AVG(monthly_hgb_value) as avg_hgb, AVG(avg_epo_dose_per_kg) as avg_epo_per_kg FROM dbo.prep_hgb_and_epo GROUP BY pid")
df.hgb_epo = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.hgb_epo, by = 'pid')
#patients_desc$monthly_hgb_value_clean = remove_outliers(patients_desc$monthly_hgb_value)
#max(patients_desc$monthly_hgb_value_clean, na.rm = TRUE)
#hospitalizations
strQuery = paste0("SELECT fk_patient as pid, COUNT(*) as num_hosp FROM fact_hospitalizations group by fk_patient")
df.hospital = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.hospital, by = 'pid')
patients_desc[is.na(patients_desc$num_hosp),"num_hosp"] = 0
#heart attacks (from diagnoses)
strQuery = paste0("SELECT fk_patient as pid, COUNT(*) as num_heart_attacks FROM fact_diagnoses where fk_icd like '410%' group by fk_patient")
df.heart = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.heart, by = 'pid')
patients_desc[is.na(patients_desc$num_heart_attacks),"num_heart_attacks"] = 0

#iron
strQuery = paste0("SELECT* FROM 
(SELECT pid, avg(CASE WHEN sum_iron_dose is null THEN 0 ELSE sum_iron_dose END) as avg_iron_monthly FROM dbo.prep_monthly_meds group by pid) monthly
LEFT join
(SELECT pid as pid2, avg(iron_last_12_months) as avg_iron_yearly,
 (CASE WHEN SUM(CASE WHEN iron_last_12_months > 5000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_5g,
 (CASE WHEN SUM(CASE WHEN iron_last_12_months > 4000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_4g,
  (CASE WHEN SUM(CASE WHEN iron_last_12_months > 3000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_3g
  FROM prep_hgb_mvg_avg_iron GROUP by pid) yearly
ON monthly.pid = yearly.pid2")
df.iron = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.iron, by = 'pid')


patients_desc %>%
  group_by(GROUP)%>%
  summarise(num_obs = length(pid),
            avg_iron_year = mean(avg_iron_yearly),
            avg_iron_month = mean(avg_iron_monthly),
            perc_above_5 = sum(once_higher_5g)/length(once_higher_5g),
            perc_above_4 = sum(once_higher_4g)/length(once_higher_4g),
            perc_above_3 = sum(once_higher_3g)/length(once_higher_3g))

patients_desc %>%
  group_by(GROUP)%>%
  summarise(num_obs = length(pid),
            avg_age=mean(ageAtDia, na.rm=TRUE), 
            avg_start_dialysis=mean(fdod, na.rm=TRUE), 
            #avg_iron = mean(avg_iron, na.rm = TRUE),
            avg_epo_dose_per_kg=mean(avg_epo_per_kg, na.rm=TRUE),
            #avg_num_high_iron = mean(high_iron, na.rm=TRUE),
            avg_deaths = mean(as.numeric(dead)-1),
            #avg_hosp = mean(num_hosp,na.rm = TRUE),
            #avg_heart_attacks = mean(num_heart_attacks,na.rm = TRUE),
            avg_hgb_level = mean(avg_hgb, na.rm=TRUE),
            avg_mths_on_dialysis = mean(as.numeric(diatime,na.rm = TRUE)))

group1 = patients[patients$GROUP==1,]
group2 = patients[patients$GROUP==2,]
summarise(patients, avg = mean(ageAtDia))
#______________________________________________#
##_____________________________________________##
#######survival analysis with two groups from SAS #####
#get groups (SAS TRAJECTORIES)
strQuery = paste0("SELECT * FROM dbo.res_hgb_traj")
groups = dbGetQuery(con, strQuery)
names(groups)[names(groups) == 'fk_patient'] <- 'pid'
#get patients
strQuery = paste0("SELECT * FROM dbo.dim_patients2 WHERE first_dialysis_lib > '2006'")
patients= dbGetQuery(con, strQuery)
#merge
patients = merge(groups, patients, by= 'pid')
#extract necessary fields
#TO DO: INTEGRATE FDOD AND FIRST TREATMENT HERE!!!
patients = patients[,c("pid","dob","dod","active_status","first_dialysis_lib","last_dialysis_lib","GROUP")]
#patients = patients[,c("pid","dob","dod","active_status","first_dialysis_lib.x","last_dialysis_lib","group")]
patients$GROUP = as.factor(patients$GROUP)
patients$dob = as.Date(patients$dob)
patients$dod = as.Date(patients$dod)
patients$fdod = as.Date(patients$first_dialysis_lib)
patients$ldod = as.Date(patients$last_dialysis_lib)
patients$dead = as.factor(ifelse(is.na(patients$dod),0,1))
patients$ageAtDia = as.numeric((patients$fdod - patients$dob))/365
x = as.Date(ifelse(is.na(patients$dod),patients$ldod,patients$dod),origin="1970-01-01") #end date of dialysis
#x = as.Date(ifelse(is.na(patients$dod),as.Date("2014-02-01"),patients$dod),origin="1970-01-01") #end date of dialysis
patients$diatime = round((x-patients$fdod)/30)
#groups$dead = patients$dead
#test = patients[patients$group!=4,]
#percentage of dead ppl in each group
#ggplot(data=patients,mapping = aes(x=GROUP, fill=dead)) + geom_bar(position="dodge")
#a =data.table(ddply(patients,c("GROUP","dead"),summarize,count=length(pid)))
#b = a[,list(dead,count,percent=(count/sum(count))*100),by=c("GROUP")]

#cat("Dead % in group")
#print(b)
#ggplot(data=b,mapping=aes(x=GROUP,y=percent,fill=dead)) +geom_bar(position="dodge",stat="identity")


#Survival Time
surv_data = data.frame(time = as.numeric(patients[,"diatime"]),
                       event = as.numeric(patients$dead)-1,
                       group = patients[,"GROUP"])

kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red"))
title("KM survival")
legend("topright", inset=.05, title="Group",
       c("lower hgb","higher hgb"), fill = c("blue","red"))

#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data)
