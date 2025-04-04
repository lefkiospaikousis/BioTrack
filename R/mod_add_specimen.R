#' add_specimen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_specimen_ui <- function(id){
  ns <- NS(id)
  
  input_width <- "80%"
  
  
  tagList(
    div(style = "font-size:13px; border: solid silver; padding: 10px", 
        shinyjs::hidden( textInput(ns('activate_observer'), NULL, value = 'some_value') ),
        tags$table(
          
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label",style = "", "Specimen Type:")),
                  tags$td(width = "60%", selectInput(ns("type"), NULL, c("", specimen_types), width = input_width))
          ),
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label", "Sample Quality:")),
                  tags$td(width = "60%", selectInput(ns("quality"), NULL, c("", col_values[["quality"]]), width = input_width))
          ),
          # FFPE specific fields (initially hidden)
          shinyjs::hidden(
            tags$tr(width = "100%", id = ns("ffpe_tumour_cellularity"),
                    tags$td(width = "30%", div(class = "input-label", style = "", col_labels[["tumour_cellularity"]])),
                    tags$td(width = "70%", textInput(ns("tumour_cellularity"), NULL, width = input_width, value = ""))
            ),
            tags$tr(width = "100%", id = ns("ffpe_surface_area"),
                    tags$td(width = "30%", div(class = "input-label",style = "", col_labels[["surface_area"]])),
                    tags$td(width = "70%", textInput(ns("surface_area"), NULL, width = input_width, placeholder = ""))
            )
          ),
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label",style = "", HTML("Date of processing"))),
                  tags$td(width = "60%", shinyjs::disabled(dateInput(ns("date_processing"), NULL, 
                                                                     lubridate::NA_Date_ , format = "dd/mm/yyyy", width = input_width) ))
                  #tags$td(width = "60%", div(htmlOutput(ns("dateProcessing"), width = input_width), style = "margin-bottom: 7px"))
          ),
          
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label", "Time of processing:")),
                  tags$td(width = "60%", shinyTime::timeInput(ns("time_processing"), NULL, seconds = FALSE))
          ),
          
          storage_placeUI(ns),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Comments:")),
                  tags$td(width = "70%", textInput(ns("comment_place"), NULL, width = input_width))),
          # FFPE specific fields (initially hidden)
          shinyjs::hidden(
            tags$tr(width = "100%", id = ns("the_n_tubes"),
                    tags$td(width = "30%", div(class = "input-label", col_labels[['n_tubes']])),
                    tags$td(width = "70%", numericInput(ns("n_tubes"), NULL, NA, min = 1, width = "40%"))
            ),
            tags$tr(width = "100%", id = ns("ffpe_blocks"),
                    tags$td(width = "30%", div(class = "input-label", col_labels[['n_blocks']])),
                    tags$td(width = "70%", numericInput(ns("n_blocks"), NULL, NA, min = 1, width = "40%"))
            ),
            tags$tr(width = "100%", id = ns("ffpe_slides"),
                    tags$td(width = "30%", div(class = "input-label", col_labels[['n_slides']])),
                    tags$td(width = "70%", numericInput(ns("n_slides"), NULL, NA, min = 1, width = "40%"))
            )
          ),
        ),
        hr(),
        actionButton(ns("submit"), "Save this specimen", class = "btn-add",
                     icon("glyphicon glyphicon-saved", lib = "glyphicon")),
        actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
                     icon("glyphicon glyphicon-remove", lib = "glyphicon"))
    )
  )
}

#' add_specimen Server Functions
#'
#' @noRd 
mod_add_specimen_server <- function(id, sample_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    # observe({
    #   browser()
    #   updateDateInput(session, "date_processing", value = as.Date(sample_info()$date_receipt) )
    #   
    #   shinyjs::toggleState("date_processing", sample_info()$tube == "Streck")
    #   
    # })|> 
    #   # this trick makes sure that the ui of this module is rendered 
    #   # before I update the date_processing. Otherwise it is not updated
    #   bindEvent(input$type, ignoreInit = TRUE)
    
    observe({
      
      req(sample_info())
      updateDateInput(session, "date_processing", value = as.Date(sample_info()$date_receipt) )
      
      shinyjs::toggleState("date_processing", condition = sample_info()$tube == "Streck")
      
      is_ffpe <- sample_info()$type1 %in% sample_types_FFPE
      is_block <- sample_info()$type1 == "FFPE Block"
      is_slide <- sample_info()$type1 == "FFPE Slide"
      is_fresh <- sample_info()$type1 == "Fresh tumour sample"
      
      if(is_ffpe){
        
        if(is_block){
          shinyjs::show("ffpe_blocks", anim = TRUE)
          shinyjs::hide("the_n_tubes", anim = TRUE)
          shinyjs::hide("ffpe_slides", anim = TRUE)
        } 
        
        if(is_slide){
          shinyjs::show("ffpe_slides", anim = TRUE)
          shinyjs::hide("the_n_tubes", anim = TRUE)
          shinyjs::hide("ffpe_blocks", anim = TRUE)
        }
        
        if(is_fresh){
          shinyjs::show("the_n_tubes", anim = TRUE)
          shinyjs::hide("ffpe_blocks", anim = TRUE)
          shinyjs::hide("ffpe_slides", anim = TRUE)
        }
        
        
        shinyjs::show("ffpe_tumour_cellularity", anim = TRUE)
        shinyjs::show("ffpe_surface_area", anim = TRUE)
        
        
        .type_choices = specimen_types[names(specimen_types) == sample_info()$type1]
        shinyjs::disable("type")
        
      } else {
        
        shinyjs::hide("ffpe_tumour_cellularity", anim = TRUE)
        shinyjs::hide("ffpe_surface_area", anim = TRUE)
        shinyjs::show("the_n_tubes", anim = TRUE)
        
        shinyjs::enable("type")
        .type_choices = c("", specimen_types[!names(specimen_types) %in% sample_types_FFPE])
        
      }
      
      updateSelectInput(session, "type", choices = .type_choices)
      
    }) |> 
      bindEvent(input$activate_observer, ignoreInit = TRUE)
    
    all_fields <- c(
      "type",
      "quality",
      "date_processing",
      "time_processing",
      "freezer",
      "rack",
      "drawer",
      "box",
      "n_tubes",
      
      "n_slides",
      "n_blocks",
      "tumour_cellularity",
      "surface_area",
      
      "comment_place"
    )
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("type", sv_required())
    iv$add_rule("freezer", sv_required())
    
    iv$add_rule("quality", sv_required())
    
    # Date of processing will be the same as date of receipt
    iv$add_rule("date_processing", sv_required())
    iv$add_rule("date_processing", function(date){
      
      # if(date > Sys.Date() ){
      #   return("Processing date cannot be later than today")
      # }
      # if(date != as.Date(sample_info()$date_receipt )){
      #   return(
      #     glue::glue("Processing date must be equal to Receipt Date{sample_info()$date_receipt}")
      #   )
      # }
      
      if( sample_info()$tube == "Streck" ) {
        if( date > (as.Date(sample_info()$date_receipt) + 5 ) ){
          return("This needs to be no later than the receipt date + 5 ")
        }
      } 
      
    })
    
    
    
    iv$add_rule("time_processing", sv_required())
    
    # Drawer -80 validation
    iv_freezer80 <- shinyvalidate::InputValidator$new()
    iv_freezer80$condition(~ input$freezer  %in% freezers_80 )
    
    iv_freezer80$add_rule("rack", sv_required())
    iv_freezer80$add_rule("box", sv_required())
    iv_freezer80$add_rule("drawer", sv_required())
    
    iv$add_validator(iv_freezer80)
    
    # Drawer -20 validation
    iv_freezer20 <- shinyvalidate::InputValidator$new()
    iv_freezer20$condition(~ input$freezer  == freezer_20 )
    
    iv_freezer20$add_rule("rack", sv_required())
    
    iv$add_validator(iv_freezer20)
    
    # # FFPE specific fields validation
    iv_ffpe_fields <- shinyvalidate::InputValidator$new()
    iv_ffpe_fields$condition(~ sample_info()$type1 %in% sample_types_FFPE)
    
    iv_ffpe_fields$add_rule("tumour_cellularity", sv_required())
    iv_ffpe_fields$add_rule("surface_area", sv_required())
    
    # Validation for FFPE slides and Blocks
    iv_n_slides <- shinyvalidate::InputValidator$new()
    iv_n_slides$condition(~ sample_info()$type1 == "FFPE Slide")
    iv_n_slides$add_rule("n_slides", sv_gt(0))
    
    iv_n_blocks <- shinyvalidate::InputValidator$new()
    iv_n_blocks$condition(~ sample_info()$type1 == "FFPE Block")
    iv_n_blocks$add_rule("n_blocks", sv_gt(0))
    
    iv_n_tubes <- shinyvalidate::InputValidator$new()
    iv_n_tubes$condition(~ !sample_info()$type1 %in% c("FFPE Block", "FFPE Slide"))
    iv_n_tubes$add_rule("n_tubes", sv_gt(0))
    
    iv_ffpe_fields$add_validator(iv_n_slides)
    iv_ffpe_fields$add_validator(iv_n_blocks)
    iv_ffpe_fields$add_validator(iv_n_tubes)
    
    iv$add_validator(iv_ffpe_fields)
    
    # Freezers ----
    
    observeEvent(input$freezer, {
      
      req(input$freezer)
      
      if(input$freezer == freezer_80_small){
        
        updateSelectInput(session, "rack", choices = freezer_internals(freezer_80_small)$rack)
        updatePrettyRadioButtons(session, "drawer", choices = freezer_internals(freezer_80_small)$drawer , inline = TRUE)
        updatePrettyRadioButtons(session, "box", choices = freezer_internals(freezer_80_small)$box, inline = TRUE)
        
      }
      
      if(input$freezer == freezer_80_big){
        
        updateSelectInput(session, "rack", choices = freezer_internals(freezer_80_big)$rack)
        updatePrettyRadioButtons(session, "drawer", choices = freezer_internals(freezer_80_big)$drawer, inline = TRUE)
        updatePrettyRadioButtons(session, "box", choices = freezer_internals(freezer_80_big)$box, inline = TRUE)
        
      }
      
      if(input$freezer == freezer_20){
        
        updateSelectInput(session, "rack", choices = freezer_internals(freezer_20)$rack)
        updatePrettyRadioButtons(session, "drawer", selected = character(0), inline = TRUE)
        updatePrettyRadioButtons(session, "box", selected = character(0), inline = TRUE)
      }
      
      if(input$freezer == freezer_04){
        
        updateSelectInput(session, "rack", selected = character(0))
        updatePrettyRadioButtons(session, "box", selected = character(0), inline = TRUE)
        updatePrettyRadioButtons(session, "drawer", selected = character(0), inline = TRUE)
        
      }
      
      shinyjs::toggleState("rack",  condition = input$freezer %in% c(freezers_80, freezer_20 ))
      shinyjs::toggleState("drawer",  condition = input$freezer %in% c(freezers_80 ))
      shinyjs::toggleState("box",  condition = input$freezer %in% freezers_80 )
      
    },ignoreInit = TRUE)
    
    
    
    
    # Data collection ----
    
    form_data <- reactive({
      
      # Temporary collection of the data 
      
      # To keep the output ids as column names - short&sweet and then
      # then possible rename using the same vector, when presenting
      all_fields <- unname(all_fields) %>% setNames(all_fields)
      
      list_dta <- map(all_fields, function(x) {
        
        # Because the Checks1 input maybe have more than one value and then it goes to
        # more than one row in the table. I need one row per submission
        if(length(input[[x]]) > 1) {
          paste(input[[x]], collapse = ", ")
        } else {
          input[[x]]
        }
        
      })
      
      
      # # because the returned value is a date-time
      list_dta$time_processing <- strftime(list_dta$time_processing, "%R")
      #list_dta$date_processing <- as.character(as.Date(sample_info()$date_receipt))
      list_dta$date_processing  <- ymd(list_dta$date_processing, tz = "EET") + hm(list_dta$time_processing)
      list_dta$time_processing <- NULL
      
      # If Date(0) object or zero length dateTime object, then needs to be NA_Date_
      # otherwise cannot be save in the DB. lubridate::is.timepoint works for both Date and Posixct objects
      # So if time_processing is not there, this will make sure all is good for saving in the DB
      list_dta <- list_dta %>% 
        purrr::map_if(lubridate::is.timepoint, function(x){
          
          if(length(x) == 0) {
            lubridate::NA_Date_
          } else {
            x
          }
        }) 
      
      list_dta
      
    }) %>% 
      bindEvent(input$submit)
    
    # date of processing should be the same as date of receipt 
    output$dateProcessing <- renderText({
      req(sample_info())
      paste0("<b>", as.Date(sample_info()$date_receipt) %>% format("%d/%m/%Y"), "<b>")
      
    })
    
    observeEvent(input$cancel, {
      
      iv$disable() # so when the user enters again, it finds all the same 
      close_form(close_form()+1)
      
    })
    
    
    observeEvent(input$submit, {
      
      if (iv$is_valid()) {
        
        iv$disable()
        removeNotification("submit_message")
        
        #shinyjs::reset("form")
        submitted(submitted()+1)
        
      } else {
        iv$enable() # Start showing validation feedback
        
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error")
      }
      
      
    })
    
    
    # Return ####
    return(
      
      list(
        dta     = form_data,
        submit  = submitted,
        cancel = close_form
      )
    )
    
  }) # End of Server
}

## To be copied in the UI
# mod_add_specimen_ui("add_specimen_1")

## To be copied in the server
# mod_add_specimen_server("add_specimen_1")
