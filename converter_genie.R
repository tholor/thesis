rm(list=ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
library(RMySQL)


con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

#get features
strQuery = paste0("SELECT pid, period, age, albumin,resulting_autonomy as autonomy, high_cci as cci, death,
                  low_bp, pre_edema, hgb, access_type, low_bmi, pneumonia, sex, hospitalization FROM 03_model_dbn where period < 24")
data = dbGetQuery(con, strQuery)
#close DB-Connection
dbDisconnect(con)
distinct_pid = unique(data$pid)
num_pids = length(distinct_pid)
max_period = max(data$period) #+1 if period 0 is included
num_vars = 13

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

#prepare & transform
# subset = data[,!names(data) %in% c("pid","period","period_of_death")]
# a=as.matrix(t(subset))
# m = matrix(data=a, nrow=num_pids, ncol=max_period*ncol(subset), byrow=TRUE)
# conv = as.data.frame(m)

start_names=names(subset(data, select = c(-period, -pid)))
all_names = start_names
for(i in 1:(max_period-1)){
  print(i)
  all_names = c(all_names, sapply(start_names, function(x) paste0(x,"_",i)))
}
names(conv) = all_names
#write back to DB
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
dbSendQuery(con, paste0("CREATE TABLE 03_model_genie5 (pid VARCHAR(50));"))
dbWriteTable(conn=con, value=conv, name="03_model_genie5",overwrite = TRUE)
#close DB-Connection
dbDisconnect(con)
