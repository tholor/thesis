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

write_all_summaries=function(){
  write_xlsx(summary_patients, "basic_R_summaries.xlsx", "patients")
  write_xlsx(summary_hemo, "basic_R_summaries.xlsx", "hemo")
  write_xlsx(summary_icds, "basic_R_summaries.xlsx", "icds")
  write_xlsx(summary_lab_types, "basic_R_summaries.xlsx", "lab_types")
  write_xlsx(summary_vasc_accesses, "basic_R_summaries.xlsx", "vasc_accesses")
  write_xlsx(summary_allergies, "basic_R_summaries.xlsx", "allergies")
  write_xlsx(summary_complications, "basic_R_summaries.xlsx", "complications")
  write_xlsx(summary_diagnoses, "basic_R_summaries.xlsx", "diagnoses")
  write_xlsx(summary_hospitalizations, "basic_R_summaries.xlsx", "hospitalizations")
  write_xlsx(summary_labs, "basic_R_summaries.xlsx", "labs")
  write_xlsx(summary_noshows, "basic_R_summaries.xlsx", "noshows")
  write_xlsx(summary_procedures, "basic_R_summaries.xlsx", "procedures")
  write_xlsx(summary_signsyms, "basic_R_summaries.xlsx", "signsyms")
  write_xlsx(summary_vitalsigns, "basic_R_summaries.xlsx", "vitalsigns")
}