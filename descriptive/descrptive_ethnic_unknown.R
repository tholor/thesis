# explore ethnic == unknown, df.preProcess needed as input from log_begin_to_1year.R
#__________________
# Desriptive: Diff. by ethnic
#_________________
#add some info 
temp = df.preProcess
#add months on dial bef dead
con = dbConnect(MySQL(), dbname='dbo', user='root', password='', host = 'localhost')
strQuery = paste0("SELECT pid, months_on_dial_bef_dead
                      FROM prep_outcomes")
queried_data = dbGetQuery(con, strQuery)
temp = merge(temp, queried_data, by="pid")
#add type of fdod
strQuery = paste0("SELECT pid, type_fdod
                      FROM dim_patients2")
queried_data = dbGetQuery(con, strQuery)
temp = merge(temp, queried_data, by="pid")
dbDisconnect(con)

temp$ethnic = ifelse(temp$ethnic=="Unknown","Unknown","Black/White")
group_by(temp, ethnic) %>%
  summarise(avg_age = mean(age),
            avg_cci = mean(cci),
            avg_survival = mean(months_on_dial_bef_dead, na.rm=TRUE),
            #avg_fdod = mean(as.Date(adj_fdod), na.rm=TRUE),
            avg_albumin = mean(albumin),
            avg_hb = mean(hgb),
            avg_calcium = mean(calcium),
            avg_creatinine = mean(creatinine))

test = subset(temp, ethnic=="Unknown")
nrow(subset(temp, ethnic!="Unknown" & access_type != "catheter_or_not_ready"))
nrow(subset(temp, ethnic=="Unknown" & type_fdod=="first_session"))


ethnic_unknown = subset(df.preProcess, ethnic=="Unknown")
description_unknown = gather(ethnic_unknown, variable, value) %>%
   count(variable, value)
description_unknown = subset(description_unknown,!variable %in% c("hgb","calcium","albumin","phosphor","ktv","potassium","calcXphosph","pth","creatinine","ferritin","phosphorus","eGFR"))
ethnic_rest = subset(df.preProcess, ethnic !="Unknown" )
 description_rest = gather(ethnic_rest, variable, value) %>%
   count(variable, value)
description_rest = subset(description_rest,!variable %in% c("hgb","calcium","albumin","phosphor","ktv","potassium","calcXphosph","pth","creatinine","ferritin","phosphorus","eGFR"))

comb = merge(description_rest, description_unknown, by=c("variable","value"))
colnames(comb) = c("variable","value","ethnic_rest", "ethnic_unknown")
comb$perc_rest = comb$ethnic_rest/608
comb$perc_unknown = comb$ethnic_unknown/129
comb$perc_difference = comb$perc_rest - comb$perc_unknown
comb = comb[order(-abs(comb$perc_difference)),]
small_description = subset(comb, !variable %in% c("hgb","calcium","albumin","phosphor","ktv","potassium","calcXphosph","pth","creatinine","ferritin","phosphorus","eGFR"))
null_description = subset(comb, is.na(value)) 

write.xlsx(description_unknown, "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\ethnic_unknown.xlsx")
write.xlsx(description_rest, "C:\\Users\\Malte\\Dropbox\\FIM\\08 Masterarbeit\\06 Coding\\R\\output\\ethnic_rest.xlsx")
