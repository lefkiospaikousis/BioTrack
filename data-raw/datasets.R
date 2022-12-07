## code to prepare `datasets` dataset goes here




col_labels <- c(
  time_stamp        = "TimeStamp of the sample info submission", # Time stamp of submission date
  unique_id         = "UNIQUE ID",
  firstname         = "Patient First Name",
  surname           = 'Patient Last Name',
  gender            = "Gender",
  bococ             = "BOCOC ID",
  dob               = 'Date of birth',
  nationality       = "Nationality",
  diagnosis         = "Diagnosis",
  status            = "Status",
  doctor            = "Referring doctor",
  consent           = "Consent Signed",
  
  type1             = "Sample Type 1",
  type1_ml          = "type1_ml",
  type2             = "Sample Type 2",
  type2_ml          = "type2_ml",
  type3             = "Sample Type 3",
  type3_ml          = "type3_ml",
  type4             = "Sample Type 4",
  type4_ml          = "type4_ml",
  type5             = "Sample Type 5",
  type5_ml          = "type5_ml",
  
  tube              = "Type of blood collection tube",
  phase             = "Clinical trial phase",
  at_bococ          = "Sample was collected at BOCOC",
  date_collection   = "Date of Collection",
  date_shipment     = 'Date of shipment',
  date_receipt      = "Date & Time of receipt",
  civil_id          = "Patient ID",
  study_id          = "Study ID",
  study             = 'Study',
  comments          = "Comments",
  
  specimens         = "Number of specimens", # how many specimens added?
  
  path_icf          = "ICF files",
  
  time_stamp2       = "TimeStamp of the sample type submission", # Time stamp of submission date
  year              = 'Year',
  serial            = "serial",
  date_processing   = "Date & Time of processing",
  duration          = "Duration from sample Collection to Processing",
  quality           = 'Sample Quality',
  specimen_type     = "Specimen type",
  lab_no            = "Lab No",
  freezer           = "Freezer",
  rack              = 'Rack',
  drawer            = "Drawer",
  box               = "Box",
  place             = 'Storage place',
  comment_place     = "Comments",
  n_tubes           = "Number of tubes"
  
)

sum(duplicated(names(col_labels)))


allowed_values <- c(
  gender = c("Male", "Female", "Other"),
  #bococ = "function",
  dob = "date",
  status = c("Metastatic", "Non metastatic"),
  consent = c("Yes", "No"),
  tube = c("EDTA", "Streck", "Sodium Heparin", "Sodium Citrate"),
  phase = c("Baseline", "Day of treatment", "Week 3", "Week 6", "Week 9", "Week 12", "End of treatment", "Other"),
  sample_types <- c("Peripheral blood", "Plasma", "Serum", "Urine", "Stools", "Bronchial aspirations"),
  at_bococ = c("Yes", "No"),
  date_collection = "datetime",
  date_shipment = "date",
  date_receipt = "datetime"
)

usethis::use_data(
  
  internal = TRUE,
  col_labels, 
  allowed_values,
  overwrite = TRUE)
