dbDisconnect(con)
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
#source("basic_descriptive.R")
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
                  prep_hgb_groups_by_range2
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
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("blue","red","green"))
title("KM survival")
legend("topright", inset=.05, title="Group", c("10-11","11-13","other"), fill = c("blue","red","green"))
#g-rho test (=log rank test)#all groups
kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("darkgreen","green","red", "coral", "blue","cornflowerblue"))
title("KM survival")
par(xpd=TRUE)
legend("bottomleft", inset=.01, title="Group",
       levels(surv_data$group), fill = c("darkgreen","green","darkred", "coral", "blue","cornflowerblue"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data)

survdiff(Surv(time,event) ~ group, data = surv_data)

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


#______________________________________
#______________________________________________#
#######survival analysis with groups from SQL (BY RANGE OF HGB AND IRON) ###
strQuery = paste0("SELECT * FROM
                   prep_hgb_groups_by_range_and_iron_median
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

#test(because high iron is only measurable after at least 12 months on dialysis => bias of survival): 
#patients = subset(patients, diatime > 12)

#Survival Time
surv_data = data.frame(time = as.numeric(patients[,"diatime"]),
                       event = as.numeric(patients$dead)-1,
                       group = patients[,"GROUP"])

#all groups
kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("darkgreen","green","red", "coral", "blue","cornflowerblue","black"))
title("KM survival")
legend("topright", inset=0, title="Group",
       levels(surv_data$group), fill = c("darkgreen","green","red", "coral", "blue","cornflowerblue","black"))
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

#______________________________________
#______________________________________________#
#######survival analysis with groups from SQL (BY RANGE OF HGB AND EPO DOSAGE) ###
strQuery = paste0("SELECT * FROM
                  prep_hgb_groups_by_range_and_epo
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


#Survival Time
surv_data = data.frame(time = as.numeric(patients[,"diatime"]),
                       event = as.numeric(patients$dead)-1,
                       group = patients[,"GROUP"])

#all groups
kmsurvival <- survfit(Surv(time,event) ~ group, data = surv_data)
summary(kmsurvival)
plot(kmsurvival, xlab="Time (months)", ylab="Survival Probability",col=c("darkgreen","green","red", "coral", "blue","cornflowerblue","black"))
title("KM survival")
legend("topright", inset=0, title="Group",
       levels(surv_data$group), fill = c("darkgreen","green","red", "coral", "blue","cornflowerblue","black"))
#g-rho test (=log rank test)
survdiff(Surv(time,event) ~ group, data = surv_data)


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
#demographics
strQuery = paste0("SELECT pid,sex,ethnic FROM dim_patients2")
df.demo= dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.demo, by = 'pid')
#comorbidities
strQuery = paste0("SELECT pid,GREATEST(diabetes_with_compl,diabetes_no_compl)as diabetes, hypertension, cci FROM 01_comorb_total")
df.comorb= dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.comorb, by = 'pid')

#iron
strQuery = paste0("SELECT* FROM 
(SELECT pid, AVG(CASE WHEN sum_iron_dose is null then 0 ELSE sum_iron_dose END) as avg_iron_monthly FROM dbo.prep_monthly_meds group by pid) monthly
LEFT join
(SELECT pid as pid2, avg(iron_last_12_months) as avg_iron_yearly,
 (CASE WHEN MAX(CASE WHEN iron_last_12_months > 5000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_5g,
 (CASE WHEN MAX(CASE WHEN iron_last_12_months > 4000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_4g,
  (CASE WHEN MAX(CASE WHEN iron_last_12_months > 3000 THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END) as once_higher_3g
  FROM prep_hgb_mvg_avg_iron GROUP by pid) yearly
ON monthly.pid = yearly.pid2")
df.iron = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.iron, by = 'pid')

#meds prescribed
strQuery = paste0("SELECT pid,statins,beta_blockers,ace_inhibitors FROM 01_meds_prescribed_total")
df.meds= dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.meds, by = 'pid')

#other labs
strQuery = paste0("SELECT * FROM 01_labs_total")
df.labs = dbGetQuery(con, strQuery)
patients_desc = left_join(patients_desc, df.labs, by = 'pid')

patients_desc %>%
  group_by(GROUP)%>%
  summarise(num_obs = length(pid),
            avg_iron_year = mean(avg_iron_yearly,na.rm=TRUE),
            avg_iron_month = mean(avg_iron_monthly,na.rm=TRUE),
            n_above_5 = sum(once_higher_5g,na.rm=TRUE),
            perc_above_5 = sum(once_higher_5g)/length(once_higher_5g),
            perc_above_4 = sum(once_higher_4g)/length(once_higher_4g),
            perc_above_3 = sum(once_higher_3g)/length(once_higher_3g))

summary.df = patients_desc %>%
  group_by(GROUP)%>%
  summarise(num_obs = length(pid),
            avg_age=mean(ageAtDia, na.rm=TRUE),
            sd_age=sd(ageAtDia, na.rm=TRUE), 
            avg_start_dialysis=mean(fdod, na.rm=TRUE), 
            sd_start_dialysis=sd(fdod, na.rm=TRUE), 
            avg_iron = mean(avg_iron_yearly, na.rm = TRUE),
            sd_iron = sd(avg_iron_yearly, na.rm = TRUE),
            avg_iron_mthly = mean(avg_iron_monthly,na.rm=TRUE),
            sd_iron_mthly = sd(avg_iron_monthly,na.rm=TRUE),
            avg_epo_dose_per_kg=mean(avg_epo_per_kg, na.rm=TRUE),
            sd_epo_dose_per_kg=sd(avg_epo_per_kg, na.rm=TRUE),
            #avg_num_high_iron = mean(high_iron, na.rm=TRUE),
            avg_deaths = mean(as.numeric(dead)-1),
            #avg_hosp = mean(num_hosp,na.rm = TRUE),
            #avg_heart_attacks = mean(num_heart_attacks,na.rm = TRUE),
            avg_hgb_level = mean(avg_hgb.x, na.rm=TRUE),
            sd_hgb_level = sd(avg_hgb.x, na.rm=TRUE),
            avg_mths_on_dialysis = mean(as.numeric(diatime),na.rm = TRUE),
            sd_mths_on_dialysis = sd(as.numeric(diatime),na.rm = TRUE),
            avg_albumin = mean(avg_albumin,na.rm = TRUE),
            sd_albumin = sd(avg_albumin, na.rm=TRUE),            
            avg_pth = mean(avg_pth,na.rm = TRUE),
            sd_pth = sd(avg_pth,na.rm = TRUE),
            avg_calcium = mean(avg_calcium,na.rm = TRUE),
            sd_calcium = sd(avg_calcium,na.rm = TRUE),
            avg_phosphorus = mean(avg_phosphorus,na.rm = TRUE),
            sd_phosphorus = sd(avg_phosphorus,na.rm = TRUE),
            avg_ktv = mean(avg_ktv,na.rm = TRUE),
            sd_ktv = sd(avg_ktv,na.rm = TRUE),
            diabetes = sum(diabetes),
            hypertension = sum(hypertension),
            perc_statins= mean(statins, na.rm=TRUE),
            n_statins= sum(statins, na.rm=TRUE),
            ace_inhibitors= mean(ace_inhibitors, na.rm=TRUE),
            beta_blockers= mean(beta_blockers, na.rm=TRUE)
            )
sd_fdod = summary.df[1,"sd_start_dialysis"]-as.Date('1970-01-01')
patients_desc %>%
  group_by(GROUP)%>%
  summarise(
    sd_albumin = sd(avg_albumin, na.rm=TRUE),
    #avg_albumin = mean(avg_albumin,na.rm = TRUE),
    #sd_albumin = sd(avg_albumin, na.rm=TRUE),            
    #avg_pth = mean(avg_pth, na.rm = TRUE),
    sd_pth = sd(avg_pth, na.rm = TRUE),
    #avg_calcium = mean(avg_calcium, na.rm = TRUE),
    sd_calcium = sd(avg_calcium, na.rm = TRUE),
    #avg_phosphorus = mean(avg_phosphorus, na.rm = TRUE),
    sd_phosphorus = sd(avg_phosphorus, na.rm = TRUE),
    #avg_ktv = mean(avg_ktv, na.rm = TRUE),
    sd_ktv = sd(avg_ktv, na.rm = TRUE)
)
patients_desc %>%
  group_by(GROUP,ethnic)%>%
    summarise(num_obs = n())
patients_desc$sex_bin = ifelse(patients_desc$sex=="F",1,0)
##t-tests 
patient_g_1 = subset(patients_desc, GROUP =="3")
patient_rest = subset(patients_desc, GROUP != "3")
t.test(as.numeric(patient_g_1$ageAtDia), as.numeric(patient_rest$ageAtDia))
t.test(patient_g_1$adj_fdod, patient_rest$adj_fdod, na.action = na.omit)
t.test(as.numeric(patient_g_1$diatime), as.numeric(patient_rest$diatime), na.action = na.omit)

t.test(patient_g_1$avg_epo_per_kg, patient_rest$avg_epo_per_kg)
t.test(patient_g_1$avg_hgb.x, patient_rest$avg_hgb.x)
t.test(patient_g_1$avg_albumin, patient_rest$avg_albumin)
t.test(patient_g_1$avg_pth, patient_rest$avg_pth)
t.test(patient_g_1$avg_calcium, patient_rest$avg_calcium)
t.test(patient_g_1$avg_phosphorus, patient_rest$avg_phosphorus)
t.test(patient_g_1$avg_ktv, patient_rest$avg_ktv)
t.test(patient_g_1$avg_iron_yearly, patient_rest$avg_iron_yearly)
t.test(patient_g_1$avg_iron_monthly, patient_rest$avg_iron_monthly)

prop.test(c(sum(patient_g_1$diabetes),sum(patient_rest$diabetes)),c(length(patient_g_1$diabetes),length(patient_rest$diabetes)))
prop.test(c(sum(patient_g_1$hypertension),sum(patient_rest$hypertension)),c(length(patient_g_1$hypertension),length(patient_rest$hypertension)))
prop.test(c(sum(patient_g_1$statins),sum(patient_rest$statins)),c(length(patient_g_1$statins),length(patient_rest$statins)))
prop.test(c(sum(patient_g_1$ace_inhibitors),sum(patient_rest$ace_inhibitors)),c(length(patient_g_1$ace_inhibitors),length(patient_rest$ace_inhibitors)))
prop.test(c(sum(patient_g_1$beta_blockers),sum(patient_rest$beta_blockers)),c(length(patient_g_1$beta_blockers),length(patient_rest$beta_blockers)))
prop.test(c(sum(patient_g_1$sex_bin),sum(patient_rest$sex_bin)),c(length(patient_g_1$sex_bin),length(patient_rest$sex_bin)))

tbl = table(patients_desc$ethnic, patients_desc$GROUP)
chisq.test(tbl)
fisher.test(tbl)
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

#-------------------------------------
#MEDIAN (Iron)
strQuery = paste0("SELECT pat.pid, avg_mthly_iron FROM prep_hgb_groups_by_range2 groups
INNER JOIN dim_patients2 pat
ON groups.fk_patient = pat.pid
LEFT JOIN (SELECT pid, AVG(CASE WHEN sum_iron_dose is null THEN 0 ELSE sum_iron_dose END) as avg_mthly_iron FROM prep_monthly_meds GROUP BY pid) meds
ON meds.pid = groups.fk_patient
WHERE epogen_flag = 'TRUE' and num_epo_taken > 20 AND adj_fdod > '2006-01-01'
LIMIT 100000;")
monthly_iron_avg = dbGetQuery(con, strQuery)
medi = median(monthly_iron_avg$avg_mthly_iron)
test = subset(monthly_iron_avg, avg_mthly_iron>medi)


#Median (Epogen)
strQuery = paste0("SELECT pat.pid, 
avg_epo_dose_per_kg 
FROM prep_hgb_groups_by_range2 groups
INNER JOIN dim_patients2 pat
ON groups.fk_patient = pat.pid
LEFT JOIN (SELECT pid, AVG(CASE WHEN avg_epo_dose_per_kg is null THEN 0 ELSE avg_epo_dose_per_kg END)as avg_epo_dose_per_kg  FROM prep_hgb_and_epo GROUP BY pid) meds
ON meds.pid = groups.fk_patient
WHERE epogen_flag = 'TRUE' and num_epo_taken > 20 AND adj_fdod > '2006-01-01'
#having avg_epo_dose_per_kg < 60
LIMIT 100000")
monthly_epo_avg = dbGetQuery(con, strQuery)
medi = median(monthly_epo_avg$avg_epo_dose_per_kg)
test = subset(monthly_epo_avg, avg_epo_dose_per_kg<73)



