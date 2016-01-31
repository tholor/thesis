# __________________________________________
# This script exports data from the data base in long format and converts it to the specific wide format needed by the GeNIE GUI for DBNs. 
# Results are exported as a file or a new table in the database
# __________________________________________
rm(list=ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
library(RMySQL)


con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

#get features
#strQuery = paste0("SELECT pid, period, age, albumin,autonomy, high_cci, death,
 #                 low_bp, pre_edema, hgb, access_type, low_bmi, pneumonia, sex, hospitalization FROM 03_model_dbn_state_0 where period < 48") # where period < 36
 #strQuery = paste0("SELECT pid, period, age, albumin,autonomy, high_cci, death,
  #                low_bp, pre_edema, hgb, access_type, low_bmi, pneumonia, sex, hospitalization, weight_gain, antihypertensive, chf, infarct, shortness_breath FROM 03_model_dbn_state_0 where period < 48") # where period < 36

#strQuery = paste0("SELECT pid, period, albumin, age, death , period_of_death FROM 03_model_dbn_state_0_no_lag where period < 4 AND pid in ('LIB0000000074','LIB0000001251','LIB0000001755','LIB0000001867','LIB0000001964')") # where period < 36
 strQuery = paste0("SELECT pid, period, age, albumin,autonomy, high_cci, death,
                 low_bp, pre_edema, hgb, potassium, access_type, low_bmi, pneumonia, hospitalization FROM 03_model_dbn_state_0 where period <= 6") # where period < 36


data = dbGetQuery(con, strQuery)
#close DB-Connection
dbDisconnect(con)
distinct_pid = unique(data$pid)
num_pids = length(distinct_pid)
max_period = max(data$period)+1 #+1 if period 0 is included
num_vars = ncol(data)-2

#prepare data frame
m = matrix(nrow=num_pids, ncol=max_period*num_vars)

for(i in 1:num_pids){
  print(i)
  current_pid = distinct_pid[i]
  #get data for pid
  temp = subset(data, pid == current_pid, select = c(-period, -pid))
  nvalues=nrow(temp)*ncol(temp)
  #put it in one row of the new matrix
  a = t(temp)
  m[i,1:nvalues] = a
}
m=apply(m,2,function(x)gsub('\\s+', '',x)) #remove all accidental whitespaces
conv = as.data.frame(m)


start_names=names(subset(data, select = c(-period, -pid)))
all_names = start_names
for(i in 1:(max_period-1)){
  print(i)
  all_names = c(all_names, sapply(start_names, function(x) paste0(x,"_",i)))
}
names(conv) = all_names
conv$pid = distinct_pid
#_________________________________
#Export results
#_________________________________
#write back to DB
#con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
#dbSendQuery(con, paste0("CREATE TABLE 03_model_genie5_state_0_small (pid VARCHAR(50));"))
#dbWriteTable(conn=con, value=conv, name="03_model_genie5_state_0_small",overwrite = TRUE)
#close DB-Connection  
#dbDisconnect(con)

#write file in the format that can be used to import dataSets to SMILE
write.table(conv, file = "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\07 Analysen\\model_comparison\\cases_6m.txt", sep="\t", quote=FALSE, na="", col.names=TRUE, row.names=FALSE)


