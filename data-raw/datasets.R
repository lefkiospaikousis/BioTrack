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
  type1_ml          = "Sample Type 1 (ml)",
  type2             = "Sample Type 2",
  type2_ml          = "Sample Type 2 (ml)",
  type3             = "Sample Type 3",
  type3_ml          = "Sample Type 3 (ml)",
  type4             = "Sample Type 4",
  type4_ml          = "Sample Type 4 (ml)",
  type5             = "Sample Type 5",
  type5_ml          = "Sample Type 5 (ml)",
  
  tube              = "Type of collection tube",
  phase             = "Timepoint of collection",
  lab               = "Histopathology lab",
  at_bococ          = "Sample was collected at BOCOC",
  date_collection   = "Date of Collection",
  date_shipment     = 'Date of shipment',
  date_receipt      = "Date & Time of receipt",
  sample_origin     = "Sample origin",
  location_lesion   = "Location of lesion",
  anatomical_site   = "Anatomical site of sampling",
  sampling_technique = "Sampling technique",
  tumour_cellularity = "Tumour cellularity",
  surface_area       = "Surface area",
  histopathology_id = "Histopathology Specimen ID",
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
  n_tubes           = "Number of tubes",
  n_blocks          = "Number of blocks",
  n_slides          = "Number of slides"
  
)

sum(duplicated(names(col_labels)))

specimen_types <- c("Peripheral blood" = "PB", 
                    "Plasma" = "PL", 
                    "Serum" = "SE", 
                    "Urine" = "UR", 
                    "Stools" = "ST", 
                    "Bronchial aspirations" = "BA",
                    "Buffy coat" = "BC",
                    "FFPE Block" = "TB",
                    "FFPE Slide" = "TS",
                    "Fresh tumour sample" = "TF"
)

type_names <- setNames(names(specimen_types), specimen_types)


freezer_80_big   <- "-80\u00B0C(B)"
freezer_80_small <- "-80\u00B0C(S)"
freezer_20       <- "-20\u00B0C"
freezer_04       <- "+4\u00B0C" 

freezers_80 <- c(freezer_80_big, freezer_80_small)

sample_types_FFPE = c("FFPE Block", "FFPE Slide", "Fresh tumour sample")
phase_FFPE    = c("Diagnosis", "After treatment", "Re-biopsy at diagnosis", "Other")

col_values <- list(
  gender        = c("Male", "Female", "Other"),
  status        = c("Metastatic", "Non metastatic"),
  consent       = c("Yes", "No"),
  tube          = c("EDTA", "Streck", "Sodium Heparin", "Sodium Citrate", "N/A"),
  phase         = c("Baseline", "Day of treatment", "Month 3", "Month 6", "Month 9", "Month 12", "End of treatment", "Other", phase_FFPE) |> unique(),
  lab           = c("NGH", "SGS Diagnostic Centre", "ECC Lab", "Dr. Pavlos Constantinou Lab", " Lysiotis Lab", "Biopsy diagnosis", "Oxinou Lab", "Other"),
  sample_types  = c("Peripheral blood", "Plasma", "Serum", "Urine", "Stools", "Bronchial aspirations", sample_types_FFPE),
  
  # FFPE
  sample_origin = c("Primary Tumour", "Metastatic lesion", "Lumph nodes"),
  sampling_technique = c("Core needle biopsy", "Fine needle aspiration biopsy", "Resection specimen", "Other"),
  
  at_bococ      = c("Yes", "No"),
  quality       = c("Good", "Heamolysed", "Thawed"),
  freezer       = c(freezer_04, freezer_20, freezers_80),
  specimen_type = names(specimen_types),
  type1         = names(specimen_types),
  type2         = names(specimen_types),
  type3         = names(specimen_types),
  type4         = names(specimen_types),
  type5         = names(specimen_types)
  
)


date_cols <- c("dob", "date_shipment")

date_time_cols <- c("date_receipt", "date_collection", "date_processing")


usethis::use_data(
  
  internal = TRUE,
  date_cols,
  date_time_cols,
  col_labels, 
  col_values,
  sample_types_FFPE,
  phase_FFPE,
  specimen_types,
  type_names,
  freezers_80,
  freezer_80_big,   
  freezer_80_small, 
  freezer_20,       
  freezer_04,       
  #freezer_internals,
  overwrite = TRUE)
