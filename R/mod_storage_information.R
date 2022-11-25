#' storage_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets awesomeCheckboxGroup radioGroupButtons prettyRadioButtons airDatepickerInput
mod_storage_information_ui <- function(id){
  ns <- NS(id)
  
  input_width <- "80%"
  
  tagList(
    
    #h3("Processing - Storage Information:", style = "text-align: center;"),
    #hr(),
    p("Storage Information for: ", htmlOutput(ns("patient_info"), inline = TRUE)),
    p("Unique ID: ", htmlOutput(ns("unique_id"), inline = TRUE)),
    hr(),
    tableOutput(ns("tbl_specimens")),
    fluidRow(
      col_4(actionButton(ns("add_specimen"), "Add a specimen", icon = icon("plus"), class = "btn-add"))
    ),
    br(),
    hr(),
    actionButton(ns("done"), "Done adding specimens", class = "btn-submit", width = "100%",
                 icon("glyphicon glyphicon-ok", lib = "glyphicon")),
    hr()
    
  )
  
}


#' storage_information Server Functions
#'
#' @noRd
mod_storage_information_server <- function(id, sample_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    rv <- reactiveValues(
      specimens = NULL
    )
    
    
    specimen_types <- c("Peripheral blood" = "PB", 
                        "Plasma" = "PL", 
                        "Serum" = "SE", 
                        "Urine" = "UR", 
                        "Stools" = "ST", 
                        "Bronchial aspirations" = "BA",
                        "Buffy coat" = "BC"
    )
    
    type_names <- setNames(names(specimen_types), specimen_types)
    
    output$tbl_specimens <- renderTable({
      
      if(is.null(rv$specimens)) validate("No specimens added yet")
      
      rv$specimens %>% 
        mutate(date_processing = date_processing %>% format("%d/%m/%Y")) %>% 
        select("Lab no"                              = lab_no,
               #"Specimen type"                       = specimen_type,
               "Sample quality"                      = quality,
               'Date & time of processing'           = date_processing, 
               #'Time from collection to processing'  = duration,
               "Freezer"                             = freezer,
               "Storage place"                       = place,
               'Number of tubes'                     = n_tubes
        )
      
    }, align = "c")
    
    output$patient_info <- renderText({
      
      req(sample_info())
      
      name <- glue::glue("BOCOC: {sample_info()$bococ}")
      
      as.character(span(name, style = 'font-weight: bold'))
      
    })
    
    output$unique_id <- renderText({
      
      req(sample_info())
      
      name <- glue::glue("{sample_info()$unique_id}")
      
      as.character(span(name, style = 'font-weight: bold'))
      
    })
    
    output$n_specimens <- renderText({
      
      # name <- glue::glue("{sample_info()$firstname} (BOCOC: {sample_info()$bococ} )")
      as.character(span(nrow(rv$specimens), style = 'font-weight: bold'))
      
    })
    
    
    observeEvent(input$add_specimen, {
      
      showModal(modalDialog(
        title = h3("Add a specimen"),
        size = "m",
        mod_add_specimen_ui(ns("add_specimen_1"), specimen_types),
        footer = NULL
      ))
    })
    
    specimen <- mod_add_specimen_server("add_specimen_1")
    
    observeEvent(specimen$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    
    # To get the submit all is good
    observeEvent(specimen$submit(), {
      
      req(sample_info())
      
      show_waiter("Processing.. Please wait")
      
      tryCatch({
        
        specimen <- specimen$dta()
        
        # If freezer != '-80' then rack = ""
        if(specimen$freezer != "-80") specimen$rack <- ""
        specimen <- map(specimen, ~ . %||% NA_character_) # If NULL then NA. Othewise glue fails
        place = glue::glue("{specimen$rack}.{specimen$drawer}.{specimen$box}")
        
        #build lab_no
        year_now <- lubridate::year(Sys.Date())
        type = specimen$type
        serial =  max_serial_year(dbase_specimen, year_now) + 1
        
        lab_no <- glue::glue("{year_now - 2000}{stringr::str_pad(serial, 4, 'left', '0')}{type}")
        
        browser()
        new_specimen <- tibble(
          
          time_stamp2       = epochTime(),
          specimen_type     = type_names[[specimen$type]],
          serial            = serial,
          quality           = specimen$quality,
          date_processing   = specimen$date_processing, # Time stamp of submission date
          duration          = lapsed_time( sample_info()$date_collection, specimen$date_processing ),
          rack              = specimen$rack,
          drawer            = specimen$drawer,
          box               = specimen$box,
          place             = place,
          lab_no            = lab_no,
          freezer           = specimen$freezer,
          n_tubes           = specimen$n_tubes,
          comment_place     = specimen$comment_place
        )
        
        # ADD Info from the sample_info before saving to DB
        new_specimen <- new_specimen %>% 
          tibble::add_column(
            year              = year_now,
            unique_id         = sample_info()$unique_id
          )
        
        
        # Add it to the rv to show to the user
        rv$specimens <- bind_rows(rv$specimens, new_specimen)
        
        
        # SAVE TO DB
        res_save <- DBI::dbAppendTable(dbase_specimen, "specimen_info", as.data.frame(new_specimen))
        
        cat("Saved ", res_save, " specimen\n")
        
        #showNotification("Saved specimen to Database!")
        show_toast("success", "", glue::glue("Specimen {new_specimen$lab_no} successfully saved!"))
        
        # rv$db_trigger <- rv$db_trigger + 1
        
        session$userData$db_trigger(session$userData$db_trigger() + 1)
        
        # Add to log
        
        bococ <- dbase_specimen %>% tbl("sample_info") %>% filter(unique_id == !!new_specimen$unique_id) %>% pull(bococ)
        info = append(new_specimen, list(bococ = bococ))
        try({add_to_logFile("Added Specimen Type", session$userData$user, info = info)}, silent = TRUE)
        
        removeModal()
        
      }, error = function(e){
        cat("Error capturing the speciment entry\n")
        print(e)
        shinyFeedback::showToast("error", title = "Error while saving the Specimen Storage info!",
                                 keepVisible = TRUE, .options = list(positionClass = "toast-top-center"),
                                 "Could not save this specimen to Database! Check your network connectivity and try again.
                                 If the problem persists, please contact support")
      })
      
      hide_waiter()
      
    }, ignoreInit = TRUE)
    
    
    # Done adding samples
    observeEvent(input$done, {
      
      if( is.null(rv$specimens) ) {
        show_toast("error", "", "You haven't added any specimens yet")
        return()
      }
      
      
      show_waiter("Saving the infomation.. Please wait", sleep = 1)
      
      # Update the DB: sample_info with number of specimens
      tryCatch({
        
        id <- sample_info()$unique_id
        n_specimens <- nrow(rv$specimens)
        
        x <- glue::glue_sql("UPDATE sample_info SET specimens = {n_specimens} WHERE unique_id = {id}", .con = dbase_specimen)
        
        rs <- DBI::dbExecute(dbase_specimen, x)
        
        cat("Updated Number of specimens for ", rs, " sample_informaton for id = ", id, "\n")
        
        if(!golem::app_prod()) showNotification("Updated sample information with number of specimens")
        
        submitted(submitted()+1)
        
      }, error = function(e){
        
        print(e)
        cat("Error when Updating the sample_info with number of samples on Database\n")
        
        # TODO Save submission locally in a dataframe?
        # Inform me?
        
        hide_waiter()
        
        show_toast("error", "Error!", 
                   "Could not save the number of specimens for the particular 'Sample Information form.'
                     However, the specimen information you have just entered are succesfully stored.
                     Take a note of the Lab no (s) and contact support!",
                   keepVisible = TRUE
        )
        
      })
      
      
    }, ignoreInit = TRUE)
    
    
    return(
      list(
        submit = submitted,
        dta = reactive(rv$specimens)
      )
    )
    
  }) # end server
}

## To be copied in the UI
# mod_storage_information_ui("storage_information_1")

## To be copied in the server
# mod_storage_information_server("storage_information_1")
