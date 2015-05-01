write_xlsx = function(data, file, sheetname) {
  #data = summary_icds
  #sheetname = "icds"
  #file = "basic_R_summaries.xlsx"
  if(file.exists(file)){
    wb = loadWorkbook(file)
    sheets = getSheets(wb)
  
    if(sheetname %in% names(sheets)){
    # update => delete sheet first
      removeSheet(wb, sheetname)
      saveWorkbook(wb, file)
    }
  }
  #then write 
  write.xlsx(data, file, sheetname, row.names = FALSE, append = TRUE, showNA = FALSE)
  
}