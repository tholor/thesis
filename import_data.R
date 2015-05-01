get_cols_of_type = function (table_name, schema, type, data_or_col_type){
  if(data_or_col_type == "data_type"){
    strQuery <- paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE table_name = '",table_name,"'   
    AND table_schema = '",schema,"'
    AND Data_type = '",type,"'")
  }else{
    strQuery <- paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE table_name = '",table_name,"'   
    AND table_schema = '",schema,"'
    AND Column_type = '",type,"'")
  }
  df.cols_wanted = dbGetQuery(con, strQuery)
  return(df.cols_wanted)
}

converted_import = function(table_name){
 # table_name = "dim_icds"
  #get full table
    strQuery = paste0("SELECT * FROM dbo.",table_name)
    df.table = dbGetQuery(con, strQuery)
  #DateTime
    cols_datetime = get_cols_of_type(table_name, "dbo", "datetime", "data_type")
    if(nrow(cols_datetime) == 1){
    df.table[,cols_datetime[,1]] = as.POSIXct(df.table[,cols_datetime[,1]])
  }else{
    df.table[,cols_datetime[,1]] = lapply(df.table[,cols_datetime[,1]], as.POSIXct)
  }
  #Integer : BUG!!
    #cols_int = get_cols_of_type(table_name, "dbo", "int", "data_type")
    #df.table[,cols_int[,1]] = lapply(df.table[,cols_int[,1]], as.integer)
  #Boolean
    cols_bool = get_cols_of_type(table_name, "dbo", "varchar(5)", "column_type")
  if(nrow(cols_bool) <2){
    df.table[,cols_bool[,1]] = as.logical(df.table[,cols_bool[,1]])
  }else{
    df.table[,cols_bool[,1]] = lapply(df.table[,cols_bool[,1]], as.logical)
  }
  #Remaining => Text (no difference between free text and factor variables)
    df.text = df.table[lapply(df.table, class) == "character"]
    #decide if it is a factor
    levels = as.data.frame(unlist(sapply(names(df.text), get_levels, "dbo", table_name)))
    levels$names = names(df.text)
    colnames(levels) = c("num_lvl","names")
    factors = levels[levels$num_lvl<50,]
    factor_names = factors$names
    #convert column format
  if(length(factor_names) != 0 ){
    if( length(factor_names) == 1 ){
      df.table[,factor_names] = factor(df.table[,factor_names])
    }else{
      df.table[,factor_names] = lapply(df.table[,factor_names], factor)
    }
  }
    str(df.table)
  return(df.table)
}

get_levels = function(col_name, schema_name, table_name ){
  strQuery = paste0("SELECT  COUNT(DISTINCT ",col_name,") + COUNT(DISTINCT case when ",col_name ," is null then 1 end) as levels 
                    FROM    ",schema_name,".",table_name)
  levels = dbGetQuery(con, strQuery)
  return(levels)
}



