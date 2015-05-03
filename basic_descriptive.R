nullToNA <- function(x) {
  x[sapply(x, is.null)] <- "No Data"
  return(x)
}

basic_summary = function(df){
 df = df.hemo
  #get all the numeric columns
    cols_numeric = sapply(df, is.numeric)
    cols_int = sapply(df, is.integer)
    df.temp_num = df[,(cols_numeric | cols_int)]
    number_of_columns = length(which(cols_numeric==TRUE | cols_int == TRUE ))
    columnnames = colnames(df)[cols_numeric|cols_int]
    type = rep("numeric", number_of_columns)
    #measures
  if(number_of_columns == 1){
    mean = mean(df.temp_num,  na.rm=TRUE)
    std = sd(df.temp_num,  na.rm=TRUE)
    max = max(df.temp_num, na.rm=TRUE)
    min = min(df.temp_num, na.rm=TRUE)
    null_values = sum(is.na(df.temp_num))
    null_perc = sum(is.na(df.temp_num)) / length(df.temp_num)
  }else {
    mean = apply(df.temp_num, 2, mean, na.rm=TRUE)
    std = apply(df.temp_num,2, sd, na.rm=TRUE)
    max = apply(df.temp_num,2, max, na.rm=TRUE)
    min = apply(df.temp_num,2, min, na.rm=TRUE)
    null_values = colSums(is.na(df.temp_num))
    null_perc = colSums(is.na(df.temp_num)) / nrow(df.temp_num)
  }
    #combining
    df.temp_num_res = data.frame(columnnames,type, mean, std,max, min, null_values,null_perc)
  ################
  #get all factor columns
    cols_fact = sapply(df, is.factor)
    df.temp_fact = (df[,cols_fact])
    number_of_columns = length(which(cols_fact==TRUE))
    columnnames = colnames(df)[cols_fact]
    df.temp_fact_res = data.frame(number_of_levels=numeric(0),name_all_levels=character(0), name_lvl_1 =character(0),
                                  obs_lvl_1=numeric(0),	name_lvl_2 = character(0),	obs_lvl_2=numeric(0),	
                                  name_lvl_3 = character(0), obs_lvl_3 = numeric(0),	name_lvl_4 = character(0),	obs_lvl_4	= numeric(0),
                                  name_lvl_5=character(0),	obs_lvl_5=numeric(0))
 
  if(number_of_columns>0){
    #measures
    type = rep("factor", number_of_columns)
    if(number_of_columns == 1){#single columns
      number_of_levels = nlevels(factor(df.temp_fact))
      name_all_levels = as.data.frame(paste(levels(df.temp_fact), sep="", collapse="; "))
      colnames(name_all_levels) = c("name_all_levels")
      top_5 = as.data.frame(t(get_top_x(df.temp_fact, 5)))
      null_values = sum(is.na(df.temp_fact))
      null_perc = sum(is.na(df.temp_fact)) / length(df.temp_fact)
     }else{ #multiple columns      
        number_of_levels = apply(df.temp_fact, 2, function(y) nlevels(factor(y)))
        name_all_levels = as.data.frame(sapply(df.temp_fact, function(y) paste(levels(y), sep="", collapse="; ")))
        colnames(name_all_levels) = c("name_all_levels")
        top_5 = as.data.frame(t(apply(df.temp_fact,2, get_top_x, 5)))
        null_values = colSums(is.na(df.temp_fact))
        null_perc = colSums(is.na(df.temp_fact)) / nrow(df.temp_fact)
      }
        #no diff. wether single or multiple
        colnames(top_5) = c("name_lvl_1", "obs_lvl_1","name_lvl_2", "obs_lvl_2","name_lvl_3", "obs_lvl_3","name_lvl_4", "obs_lvl_4","name_lvl_5", "obs_lvl_5")
        #TO DO: other_levels summary

      #combining
        df.temp_fact_res = data.frame(columnnames,type,null_values,null_perc,number_of_levels,name_all_levels, top_5)
    }
  #######################
  # get all free text columns
  # probably. still a bug /  counting factors as text? 
  cols_text= sapply(df, is.character)
  df.temp_text = df[,cols_text]
  columnnames = colnames(df)[cols_text]
  type = rep("text", ncol(df.temp_text))
  null_values = colSums(is.na(df.temp_text))
  null_perc = colSums(is.na(df.temp_text)) / nrow(df.temp_text)
  df.temp_text_res = data.frame(columnnames, type,null_values,null_perc)
  ##################
  #get all date columns (approach: POSIXct is double but not numeric in R)
    cols_double = sapply(df, is.double)
    cols_non_numeric = !(sapply(df, is.numeric))
    cols_dates = Reduce("&", list(cols_double, cols_non_numeric))
    df.temp_doub = df[,cols_dates]
    number_of_columns = length(which(cols_dates))
    columnnames = colnames(df)[cols_dates]
    type = rep("date", number_of_columns)  
    df.temp_doub_res = data.frame(mean_date=numeric(0), max_date=numeric(0),min_date=numeric(0))
    #measures
    if(number_of_columns == 1){
      mean_date = mean(df.temp_doub, na.rm=TRUE)
      max_date = max(df.temp_doub, na.rm=TRUE)
      min_date = min(df.temp_doub, na.rm=TRUE)
      null_values = sum(is.na(df.temp_doub))
      null_perc = sum(is.na(df.temp_doub)) / length(df.temp_doub) 
      df.temp_doub_res = data.frame(columnnames, type, mean_date, max_date, min_date, null_values,null_perc)
    }else if(number_of_columns > 1){
      mean_date = t(as.data.frame(lapply(df.temp_doub, mean, na.rm=TRUE)))
      colnames(mean_date) = c("mean_date")
      max_date = apply(df.temp_doub,2, max, na.rm=TRUE)
      min_date = apply(df.temp_doub,2, min, na.rm=TRUE)
      null_values = colSums(is.na(df.temp_doub))
      null_perc = colSums(is.na(df.temp_doub)) / nrow(df.temp_doub)
      df.temp_doub_res = data.frame(columnnames, type, mean_date, max_date, min_date, null_values,null_perc)    
    }
  #combining
  #####
  #get all Booleans
  cols_bool = sapply(df, is.logical)
  df.temp_bool = df[,cols_bool]
  number_of_columns = length(which(cols_bool==TRUE))
  columnnames = colnames(df)[cols_bool]
  type = rep("Boolean", number_of_columns)
  df.temp_bool_res = data.frame(num_true=numeric(0),num_false=numeric(0))
  #measures
  if(number_of_columns == 1){
    num_true = length(subset(df.temp_bool, df.temp_bool ==TRUE))
    num_false = length(subset(df.temp_bool, df.temp_bool ==FALSE))
    null_values = sum(is.na(df.temp_bool))
    null_perc = sum(is.na(df.temp_bool)) / length(df.temp_bool)
    df.temp_bool_res = data.frame(columnnames, type, num_true, num_false, null_values,null_perc)  
  }else if(number_of_columns > 1){
    num_true = apply(df.temp_bool, 2,function(y)length(which(y == TRUE)))
    num_false = apply(df.temp_bool, 2,function(y)length(which(y == FALSE)))
    null_values = colSums(is.na(df.temp_bool))
    null_perc = colSums(is.na(df.temp_bool)) / nrow(df.temp_bool)
    #df.temp_bool_res = data.frame()    
    df.temp_bool_res = data.frame(columnnames, type, num_true, num_false, null_values,null_perc)  
  }
  #combining
  ##TO DO: MAKE SURE THAT NOW ALL COLUMNS HAVE BEEN CATCHED AND THAT THERE ARE NO OTHER TYPES 
  #################################
  #combining final data frame
  df.final = rbind.fill(df.temp_num_res, df.temp_doub_res, df.temp_fact_res,df.temp_text_res, df.temp_bool_res)
  return (df.final)
}

#returns the top x level of a column (name1, number1, name2, number2 ...)
get_top_x = function(column, x){
 #x = 5
 #column = df.temp_fact$ethnicity
  df = as.data.frame(table(column))
  if(dim(df)[1] != 0){
    df = df[order(-df$Freq),]
    df = df[1:5,]
    vect = c(t((df)))
  }else{
    vect = c(rep("No Data",10))
  }
  return(vect)
}
