rm(list = ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
source("initialize_libraries.R")
source("models\\helper_functions.R")
#______________________________________
#SETTINGS
#______________________________________

# IMPORT DATA #
#______________________________________
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT de.pid, de.dod, de.from_date, de.to_date FROM 02_cohort_01 coh 
                  INNER JOIN 01_death_periods de ON coh.pid = de.pid")
data = dbGetQuery(con, strQuery)
#close DB-Connection
dbDisconnect(con)

#_____________________________________
# Show distribution of mortality day
# ____________________________________
data$dod = as.Date(data$dod)
data$from_date = as.Date(data$from_date)
data$survived_days_in_period = difftime( data$dod,data$from_date, units=c("days"))
data = data[complete.cases(data),]
data$day_of_month = format(as.Date(data$dod),"%d")
data$month = format(as.Date(data$dod),"%m")
data$weekday = format(as.Date(data$dod),"%w")
data$weekday_name = weekdays(data$dod)
#over time
hist(data$dod, breaks= 100, freq=TRUE)

#within year
#hist(as.numeric(data$month), breaks = 13)
ggplot(data=as.data.frame(table(data$month)), aes(Var1,Freq))+geom_bar(stat="identity")+xlab("Month")+ggtitle("Within a year")

#within month
ggplot(data=as.data.frame(table(data$day_of_month)), aes(Var1,Freq))+geom_bar(stat="identity")+xlab("Day")+ggtitle("Within month")
#within 30-day-period
ggplot(data=as.data.frame(table(data$survived_days_in_period)), aes(Var1,Freq))+geom_bar(stat="identity")+ggtitle("Within 30-day-period")+xlab("Day")

test = filter(data, month == "00")
table(data$month)
#within week
temp = as.data.frame(table(data$weekday_name))
temp$Var1 = as.ordered(temp$Var1)
temp$Var1 = reorder(temp$Var1, c(2,4,5,3,1,6,7))
temp = temp[order(temp$Var1),]
ggplot(data=temp, aes(Var1,Freq))+geom_bar(stat="identity")+xlab("Day")+ggtitle("Within a week")
