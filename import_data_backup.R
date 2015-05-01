

get_patients = function(){
  strQuery <- "SELECT * FROM dbo.dim_patients"
  df.dim_patients = dbGetQuery(con, strQuery)
  #convert datetimes
  df.dim_patients$dob= as.POSIXct(df.dim_patients$dob)
  df.dim_patients$approx_dob= as.POSIXct(df.dim_patients$approx_dob)
  df.dim_patients$dod= as.POSIXct(df.dim_patients$dod)
  df.dim_patients$creation_date= as.POSIXct(df.dim_patients$creation_date)
  df.dim_patients$modified_date= as.POSIXct(df.dim_patients$modified_date)
  #convert factors
  columnsToConvert= c("sex", "ethnic", "ethnicity", "city", "state", "zip", "marital", "religion", "county", "active_status", "emp_status", "student_status", "education", "place_of_death")
  df.dim_patients[,columnsToConvert] = lapply(df.dim_patients[,columnsToConvert], factor)
  #convert booleans
  df.dim_patients$active = as.logical(df.dim_patients$active)
  df.dim_patients$living_will = as.logical(df.dim_patients$living_will)
  df.dim_patients$living_will_on_file = as.logical(df.dim_patients$living_will_on_file) 
  return (df.dim_patients)
}

get_patients_old = function(){
  strQuery <- "SELECT * FROM dbo.dim_patients"
  df.dim_patients = dbGetQuery(con, strQuery)
  #convert datetimes
  df.dim_patients$dob= as.POSIXct(df.dim_patients$dob)
  df.dim_patients$approx_dob= as.POSIXct(df.dim_patients$approx_dob)
  df.dim_patients$dod= as.POSIXct(df.dim_patients$dod)
  df.dim_patients$creation_date= as.POSIXct(df.dim_patients$creation_date)
  df.dim_patients$modified_date= as.POSIXct(df.dim_patients$modified_date)
  #convert factors
  columnsToConvert= c("sex", "ethnic", "ethnicity", "city", "state", "zip", "marital", "religion", "county", "active_status", "emp_status", "student_status", "education", "place_of_death")
  df.dim_patients[,columnsToConvert] = lapply(df.dim_patients[,columnsToConvert], factor)
  #convert booleans
  df.dim_patients$active = as.logical(df.dim_patients$active)
  df.dim_patients$living_will = as.logical(df.dim_patients$living_will)
  df.dim_patients$living_will_on_file = as.logical(df.dim_patients$living_will_on_file) 
  return (df.dim_patients)
}

get_hemo_old = function () {
  strQuery <- "SELECT * FROM dbo.dim_hemodialyses"
  df.hemo = dbGetQuery(con, strQuery)
  #convert datetimes
  df.hemo$date= as.POSIXct(df.hemo$date)
  df.hemo$end_time= as.POSIXct(df.hemo$end_time)
  df.hemo$sig_date= as.POSIXct(df.hemo$sig_date)
  df.hemo$setup_date_time= as.POSIXct(df.hemo$setup_date_time)
  df.hemo$creation_date= as.POSIXct(df.hemo$creation_date)
  df.hemo$modified_date= as.POSIXct(df.hemo$modified_date)
  df.hemo$prehd_date_time= as.POSIXct(df.hemo$prehd_date_time)
  df.hemo$machine2_time_changed= as.POSIXct(df.hemo$machine2_time_changed)  
  df.hemo$posthd_date_time= as.POSIXct(df.hemo$posthd_date_time)
  #convert factors
  columnsToConvert= c("dialysate_name", "dialyzer_type_name", "mobility_in", "dialyzer_type2_name", "dialysate2_name", "catheter_instillation_name",
                      "decrease_reason", "mobility_out", "discharge_dest", "dialysis_type")
  df.hemo[,columnsToConvert] = lapply(df.hemo[,columnsToConvert], factor)
  #convert booleans
  columnsToConvert= c("isol", "pre_pain_present", "respirator", "pre_unable_to_weigh","post_pain_present", "ignore_for_billing", 
                      "ultrafiltration_only", "patient_confirmed", "not_a_treatment", "hemodiafiltration", "post_unable_to_weigh",
                      "admit_from_hospital", "alarms_test","dialyzer_pressure_test", "pre_pulse_rhythm_regular", "pre_breath_sounds_vesicular",
                      "pre_edema_absent", "pre_mental_status_oriented", "pre_prim_access_site_intact", "pre_bruit_present", "pre_thrill_present",
                      "post_pulse_rhythm_regular", "post_breath_sounds_vesicular","post_edema_absent", "post_mental_status_oriented", 
                      "post_prim_access_site_intact", "post_bruit_present", "post_thrill_present","blood_returned", "dressing_applied", 
                      "catheter_care_completed", "dialysis_time_decreased" )
  df.hemo[,columnsToConvert] = lapply(df.hemo[,columnsToConvert], as.logical)
  return(df.hemo)
}