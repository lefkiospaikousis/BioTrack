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
    div(style = "font-size:13px",
        #hr(style = "width: 80%"),
        tags$table(
          
          # tags$tr(width = "100%",
          #         tags$td(width = "40%", div(class = "input-label",style = "", HTML("Date & Time<br>of processing"))),
          #         
          #         tags$td(width = "60%",
          #                 
          #                 splitLayout(cellWidths = c("50%", "50%"),
          #                             div(dateInput(ns("date_processing"), NULL, "", width = input_width)),
          #                             div(style = "margin-top: 1px" , shinyTime::timeInput(ns("time_processing"), NULL, seconds = FALSE)),
          #                 ))),
          
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label", "Sample Quality:")),
                  tags$td(width = "10%", div(class = "input-label", "")),
                  tags$td(width = "60%", selectInput(ns("quality"), NULL, c("", "Good", "Heamolysed", "Thawed"), width = "100%")))
          
          
        )
    ),
    hr(),
    fluidRow(
      col_4(actionButton(ns("add_specimen"), "Add a specimen", icon = icon("plus"), class = "btn-add"))
    ),
    br(),
    tableOutput(ns("tbl_specimens")),
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
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("quality", sv_required())
    
    
    specimen_types <- c("Peripheral blood" = "PB", 
                        "Plasma" = "PL", 
                        "Serum" = "SR", 
                        "Urine" = "UR", 
                        "Stools" = "ST", 
                        "Bronchial aspirations" = "BA"
    )
    
    type_names <- setNames(names(specimen_types), specimen_types)
    
    output$tbl_specimens <- renderTable({
      
      if(is.null(rv$specimens)) validate("No specimens added yet")
      
      rv$specimens %>% 
        select(-serial) %>% 
        tibble::rowid_to_column("Serial") %>% 
        select("Lab no" = lab_no,
               "Specimen type" = specimen_type,
               "Freezer" = freezer,
               "Storage place" = place,
               'Number of tubes'= n_tubes
        )
      
    }, align = "c")
    
    output$patient_info <- renderText({
      
      req(sample_info())
      
      name <- glue::glue("{sample_info()$firstname} (BOCOC: {sample_info()$bococ} )")
      
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
        
        lab_no <- glue::glue("{year_now - 2000}{stringr::str_pad(serial, 5, 'left', '0')}{type}")
        
        
        new_specimen <- tibble(
          
          specimen_type     = type_names[[specimen$type]],
          serial            = serial,
          rack              = specimen$rack,
          drawer            = specimen$drawer,
          box               = specimen$box,
          place             = place,
          lab_no            = lab_no,
          freezer           = specimen$freezer,
          n_tubes           = specimen$n_tubes
        )
        
        # ADD Info from the sample_info before saving to DB
        new_specimen <- new_specimen %>% 
          tibble::add_column(
            
            year              = year_now,
            unique_id         = sample_info()$unique_id,
            quality           = input$quality,
            duration          = lapsed_time( sample_info()$date_receipt, Sys.time() ),
            date_processing   = epochTime() # Time stamp of submission date
          )
        
        
        # Add it to the rv to show to the user
        rv$specimens <- bind_rows(rv$specimens, new_specimen)
        
        
        # SAVE TO DB
        res_save <- DBI::dbAppendTable(dbase_specimen, "specimen_info", as.data.frame(new_specimen))
        
        cat("Saved ", res_save, " specimen\n")
        
        showNotification("Saved specimen to Database!")
        
        rv$db_trigger <- rv$db_trigger + 1
        
        session$userData$db_trigger(session$userData$db_trigger() + 1)
        
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
        show_toast("error", "Error", "You haven't added any specimens yet")
        return()
      }
      
      if (!iv$is_valid()) {
        
        iv$enable() # Start showing validation feedback
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
      if (iv$is_valid()) {
        
        iv$disable()
        
        removeNotification("submit_message")
        
        show_waiter("Saving the infomation.. Please wait", sleep = 1)
        
        # Update the DB: sample_info with number of specimens
        tryCatch({
         
          id <- sample_info()$unique_id
          n_specimens <- nrow(rv$specimens)
          
          x <- glue::glue_sql("UPDATE sample_info SET specimens = {n_specimens} WHERE unique_id = {id}", .con = dbase_specimen)
          
          rs <- DBI::dbExecute(dbase_specimen, x)
          
          cat("Updated Number of specimens for ", rs, " sample_informaton for id = ", id, "\n")
          
          if(!golem::app_prod()) showNotification("Updated sample information with number of specimens")
          
           hide_waiter()
           Sys.sleep(0.5)
           show_toast("success", "Done saving!", "Specimens successfully saved!")
           Sys.sleep(2)
          
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
        
        
      } 
      
      
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
