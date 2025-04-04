#' storage_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets awesomeCheckboxGroup radioGroupButtons prettyRadioButtons airDatepickerInput updatePrettyRadioButtons
mod_storage_information_ui <- function(id){
  ns <- NS(id)
  
  input_width <- "80%"
  
  tagList(
    p("Specimen storage Information for: ", htmlOutput(ns("patient_info"), inline = TRUE)),
    hr(),
    tableOutput(ns("tbl_specimens")),
    fluidRow(
      col_4(actionButton(ns("add_specimen"), "", icon = icon("plus"), class = "btn-add"))
    ),
    br(),
    div(id  = ns("placeholder")),
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
#' @param sample_info A list of th sample information
mod_storage_information_server <- function(id, sample_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    
    rv <- reactiveValues(
      specimens = NULL
    )
    
    observe({
      rv$specimens <- NULL
      
      if(sample_info()$type1 %in% sample_types_FFPE){
        .label = glue::glue("Add a {sample_info()$type1}")
      } else {
        .label = "Add a specimen"
      }
      
      updateActionButton(session, "add_specimen", label = .label)
      
    }) |> 
      bindEvent(sample_info())
    
    output$tbl_specimens <- renderTable({
      
      if(is.null(rv$specimens)) validate("No specimens added yet")
      
      rv$specimens %>% 
        mutate(date_processing = date_processing %>% format("%d/%m/%Y")) %>% 
        transmute("Lab no"                              = lab_no,
                  #"Specimen type"                       = specimen_type,
                  "Sample quality"                      = quality,
                  'Date & time of processing'           = date_processing, 
                  #'Time from collection to processing'  = duration,
                  "Freezer"                             = glue::glue("{freezer}({place})"),
                  #"Storage place"                       = place,
                  'Number of tubes'                     = n_tubes,
                  'Number of slides'                    = n_slides,
                  'Number of blocks'                    = n_blocks
        )
      
    }, align = "c")
    
    output$patient_info <- renderText({
      
      req(sample_info())
      
      if(!is.na(sample_info()$type1_ml)){
        
        ml <- paste0(sample_info()$type1_ml, " ml")
      } else {
        ml = ""
      }
      
      name <- glue::glue("BOCOC: {sample_info()$bococ} / Sample: {sample_info()$type1} {ml}")
      
      as.character(span(name, style = 'font-weight: bold'))
      
    })
    
    output$unique_id <- renderText({
      
      req(sample_info())
      
      name <- glue::glue("{sample_info()$unique_id}")
      
      as.character(span(name, style = 'font-weight: bold'))
      
    })
    
    output$n_specimens <- renderText({
      
      as.character(span(nrow(rv$specimens), style = 'font-weight: bold'))
      
    })
    
    counter_specimen <- reactiveVal(0)
    
    observeEvent(input$add_specimen, {
      
      counter_specimen(counter_specimen() + 1)

      id_module = paste0("add_specimen_", counter_specimen())
      id_remove = paste0("to_remove_", counter_specimen())
      
      insertUI(
        paste0("#", ns("placeholder")),
        ui = div(id = ns(id_remove), mod_add_specimen_ui(ns(id_module)) ),
        where = "afterEnd"
      )
      
      shinyjs::disable("add_specimen")
      
      
      specimen <- mod_add_specimen_server(id_module, sample_info)
      
      observeEvent(specimen$cancel(), {
        
        removeUI( paste0("#", ns(id_remove))    )
        
        shinyjs::enable("add_specimen")
        
      }, ignoreInit = TRUE)
      
      
      # To get the submit all is good
      observeEvent(specimen$submit(), {
        
        req(sample_info())
        
        show_waiter("Processing.. Please wait")
        
        tryCatch({
          
          specimen <- specimen$dta()
          
          if(specimen$freezer == freezer_04) {
            
            specimen$rack   <- ""
            specimen$drawer <- ""
            specimen$box    <- ""
            
          }
          
          if(specimen$freezer == freezer_20) {
            
            specimen$box    <- ""
            specimen$drawer <- ""
            
          }
          
          specimen <- map(specimen, ~ . %||% NA_character_) # If NULL then NA. Otherwise glue fails
          place = glue::glue("{specimen$rack}.{specimen$drawer}.{specimen$box}")
          
          #build lab_no
          year_now <- lubridate::year(Sys.Date())
          type = specimen$type
          serial =  max_serial_year(dbase_specimen, year_now) + 1
          
          lab_no <- glue::glue("{year_now - 2000}{stringr::str_pad(serial, 4, 'left', '0')}{type}")
          
          
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
            n_slides          = specimen$n_slides,
            n_blocks          = specimen$n_blocks,
            surface_area      = specimen$surface_area,
            tumour_cellularity = specimen$tumour_cellularity,
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
          
          show_toast("success", "", glue::glue("Specimen {new_specimen$lab_no} successfully saved!"))
          
          session$userData$db_trigger(session$userData$db_trigger() + 1)
          
          # Add to log
          bococ <- sample_info()$bococ
          info = append(new_specimen, list(bococ = bococ))
          try({add_to_logFile("Added Specimen Type", session$userData$user, info = info)}, silent = TRUE)
          
          removeUI( paste0("#", ns(id_remove))    )
          shinyjs::enable("add_specimen")
          
        }, error = function(e){
          cat("Error capturing the speciment entry\n")
          print(e)
          shinyFeedback::showToast("error", title = "Error while saving the Specimen Storage info!",
                                   keepVisible = TRUE, .options = list(positionClass = "toast-top-center"),
                                   "Could not save this specimen to Database! Check your network connectivity and try again.
                                 If the problem persists, please contact support")
          shinyjs::enable("add_specimen")
        })
        
        hide_waiter()
        
      }, ignoreInit = TRUE)
      
      
    })
    # Done adding samples
    observeEvent(input$done, {
      
      if( is.null(rv$specimens) ) {
        show_toast("error", "", "You haven't added any specimens yet")
        return()
      }
      
      submitted(submitted()+1)
      
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
