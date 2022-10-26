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
  
  sample_types <- c("Peripheral blood", "Plasma", "Serum", "Urine", "Stools", "Bronchial aspirations")
  
  
  tagList(
    
    h3("Processing - Storage Information:", style = "text-align: center;"),
    hr(),
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
      col_4(h4("Specimens added: ", htmlOutput(ns("n_specimens"), inline = TRUE))),
      col_4(actionButton(ns("add_specimen"), "Add a specimen", icon = icon("plus"), class = "btn-add"))
    ),
    hr(),
    actionButton(ns("done"), "Done adding samples", class = "btn-submit", width = "100%",
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
    
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("quality", sv_required())
    
    
    
    rv <- reactiveValues(
      specimens = tibble::tibble()
    )
    
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
    
    
    mod_add_specimen_server("add_specimen_1")
    
    observeEvent(input$add_specimen, {
      
      showModal(modalDialog(
        title = h4("Add a specimen type"),
        size = "m",
        mod_add_specimen_ui(ns("add_specimen_1")),
        footer = NULL
      ))
    })
    
    specimen <- mod_add_specimen_server("add_specimen_1")
    
    observeEvent(specimen$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    
    # To get the submit all is good
    observeEvent(specimen$submit(), {
      
      show_waiter("Processing.. Please wait")
      removeModal()
      
      tryCatch({
        
        specimen <- specimen$dta()
        
        # If freezer != '-80' then rack = ""
        if(specimen$freezer != "-80") specimen$rack <- ""
        place = glue::glue("{specimen$rack}.{specimen$drawer}.{specimen$box}")
        
        #build lab_no
        yy <- lubridate::year(Sys.Date()) - 2000
        type = "PL" # specimen_code(specimen_type)
        serial =  1 # serial_specimen()
        
        lab_no <- glue::glue("{yy}{stringr::str_pad(serial, 5, 'left', '0')}{type}")
        
        new_specimen <- tibble(
          specimen_type     = specimen$type,
          serial            = serial,
          place             = place,
          lab_no            = lab_no,
          freezer           = specimen$freezer,
          n_tubes           = specimen$n_tubes
        )
        
        rv$specimens <- bind_rows(rv$specimens, new_specimen)
        
      })
      
      hide_waiter()
    }, ignoreInit = TRUE)
    
    
    
    # Done adding samples
    observeEvent(input$done, {
      
      
    })
    
    observeEvent(input$done, {
      
      if( nrow(rv$specimens) == 0) {
        shinyFeedback::showToast("error", "You haven't added any specimens yet")
        return()
      }
      
      if (iv$is_valid()) {
        
        iv$disable()
        
        removeNotification("submit_message")
        
        browser()
        rv$specimens %>% 
          tibble::add_column(
            # for all specimens
            unique_id         = sample_info()$unique_id,
            quality           = input$quality,
            duration          = lapsed_time(sample_info()$date_receipt, Sys.time() ),
            date_processing   = epochTime(), # Time stamp of submission date
            
          ) %>% 
          glimpse()
        
        submitted(submitted()+1)
        
      } else {
        
        iv$enable() # Start showing validation feedback
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    
    
  })
}

## To be copied in the UI
# mod_storage_information_ui("storage_information_1")

## To be copied in the server
# mod_storage_information_server("storage_information_1")
