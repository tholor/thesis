rm(list=ls())
gc()
path= "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R"
setwd(path)
library(RMySQL)
library(ICC)

con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')

#get features
#strQuery = paste0("SELECT pid, period, age, albumin,autonomy, high_cci, death,
#                 low_bp, pre_edema, hgb, access_type, low_bmi, pneumonia, sex, hospitalization FROM 03_model_dbn_state_0 where period < 48") # where period < 36
#strQuery = paste0("SELECT pid, period, age, albumin,autonomy, high_cci, death,
#                low_bp, pre_edema, hgb, access_type, low_bmi, pneumonia, sex, hospitalization, weight_gain, antihypertensive, chf, infarct, shortness_breath FROM 03_model_dbn_state_0 where period < 48") # where period < 36

#  strQuery = paste0("SELECT mod1.*, mod2.* FROM 
#                    03_model_dbn_state_0 mod1 LEFT JOIN 
#                    03_model_dbn_state_0 mod2 ON mod1.pid = mod2.pid AND mod1.period = mod2.period+1 AND mod1.period < 36")
strQuery = paste0("SELECT * FROM 03_model_dbn_state_0 WHERE period < 36") # for approach one

data = dbGetQuery(con, strQuery)
#close DB-Connection
dbDisconnect(con)


vars = names(data)[!names(data) %in% c("pid", "period", "death", "period_of_death", "stabilizing", "time_dialysis")]



#transition probabilities
#number of possible states
trans = data.frame(prob = rep(0, length(vars)))
row.names(trans) = vars
# states["albumin",] = 4
# states["hgb",] = 4
# states["age",] = 4
# states["autonomy",] = 3

for(name in vars){
  show(name)
  states = length(unique(data[[name]][!is.na(data[[name]])]))
  p = matrix(nrow = states, ncol = states, 0)
  i=0
  for(pat in unique(data$pid)){
    i=i+1
   #pat = "LIB0000001387"
    x = subset(data, pid == pat, select = name)[,1]
    
    for (t in 1:(length(x) - 1)) {
      p[x[t]+1, x[t + 1]+1] <- p[x[t]+1, x[t + 1]+1] + 1
    }
  }
  #normalize
  for (i in 1:states) p[i, ] <- p[i, ] / max(sum(p[i, ]),1)
  show(p)
  
  #P(state_t = x | state_t-1 = x)
  #sum of diagonal / states
  trans[name,"prob"] = sum(diag(p)) / states
  show( trans[name,"prob"])
}
trans_ord = trans
trans_ord$name = row.names(trans)
trans_ord = trans_ord[order(-trans_ord$prob),]

#####################
#Aproach 2
#intraclass correlation
iccs = data.frame(var = vars, icc = NA)
for(name in vars){
  iccs[iccs$var==name,"icc"] = ICCest(pid, name, data)$ICC
}
#icc_albumin = ICCest(pid, albumin, data)
iccs_ord = iccs[order(-iccs$icc),]
#iccs = lapply(data, function (x) mean(x, na.rm=TRUE))

####################################
#Approach 3: stand. dev
sds = as.data.frame(matrix(, nrow= length(unique(data$pid)), ncol = length(vars)+1))
names(sds) = c("pid",vars)

#standardize
data_stand = data 
data_stand$albumin = data$albumin/(4/2)
data_stand$age = data$age/(4/2)
data_stand$hgb = data$hgb/(4/2)
data_stand$autonomy = data$autonomy/(3/2)

i=0
for(pat in unique(data$pid)){
  i=i+1
  #pat = "LIB0000000074"
  sub = subset(data_stand, pid == pat, select = vars)
  sds[i,1] = pat
  #autocorr = acf(sub$albumin, lag.max = 1, na.action = na.pass, type = "correlation")
  #autocorr$acf
  sds[i,2:dim(sds)[2]] = as.data.frame(lapply(sub, function (x) sd(x, na.rm=TRUE))) 
}
avg_sds = as.data.frame(sapply(sds, function(x) mean(x, na.rm = TRUE)))
names(avg_sds) = c("sd")
avg_sds$name = row.names(avg_sds)
sds_ord = avg_sds[order(-avg_sds$sd),]
