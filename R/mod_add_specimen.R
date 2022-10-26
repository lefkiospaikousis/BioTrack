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
  
  sample_types <- c("Peripheral blood", "Plasma", "Serum", "Urine", "Stools", "Bronchial aspirations")
  
  
  tagList(
 
    div(style = "font-size:13px",
        hr(style = "width: 80%"),
        tags$table(
          
          tags$tr(width = "100%",
                  tags$td(width = "40%", div(class = "input-label",style = "", "Specimen Type:")),
                  tags$td(width = "60%", selectInput(ns("type"), NULL, 
                                                     c("", sample_types), 
                                                     width = input_width))),
          
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label",style = "", "Freezer:")),
                  tags$td(width = "70%", prettyRadioButtons(ns("freezer"), NULL, c("-80", "-20", "+4"), 
                                                            inline = TRUE,  fill = TRUE, selected = character(0), width = "100%"))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", h4("Storage place"))),
          
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Rack:")),
                  tags$td(width = "70%", shinyjs::disabled(
                    prettyRadioButtons(ns("rack"), NULL, c("A", "B", "C", "D"), 
                                       fill = TRUE, inline = TRUE, selected = character(0), width = "100%"))
                  )),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Drawer:")),
                  tags$td(width = "70%", prettyRadioButtons(ns("drawer"), NULL, c(1:5), 
                                                            inline = TRUE,  fill = TRUE, selected = character(0), width = "100%"))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Box:")),
                  tags$td(width = "70%", prettyRadioButtons(ns("box"), NULL, c(1:3), 
                                                            inline = TRUE,  fill = TRUE, selected = character(0), width = input_width))),
          
          tags$tr(width = "100%",
                  tags$td(width = "30%", div(class = "input-label", "Number of tubes:")),
                  tags$td(width = "70%", numericInput(ns("n_tubes"), NULL, NA, min = 1, width = "40%"))),
          
        )
    ),
    hr(),
    actionButton(ns("submit"), "Done", class = "btn-submit",
                 icon("glyphicon glyphicon-ok", lib = "glyphicon")),
    actionButton(ns('cancel'), "Cancel", class = "btn-cancel right", 
                 icon("glyphicon glyphicon-remove", lib = "glyphicon")),
    hr()
  )
}
    
#' add_specimen Server Functions
#'
#' @noRd 
mod_add_specimen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    submitted <- reactiveVal(0)
    close_form <- reactiveVal(0)
    
    all_fields <- c(
      "type",
      "freezer",
      "rack",
      "drawer",
      "box",
      "n_tubes"
    )
    
    # Validation ----
    
    iv <- shinyvalidate::InputValidator$new()
    
    iv$add_rule("type", sv_required())
    iv$add_rule("freezer", sv_required())
    iv$add_rule("drawer", sv_required())
    iv$add_rule("n_tubes", sv_gt(0))
    
    # iv$add_rule("time_incident", function(time){
    #   if(identical(strftime(time, "%R"), "00:00")){
    #     "Required: (ωω:λλ)"
    #   }
    # })
    
    iv_freezer <- shinyvalidate::InputValidator$new()
    iv_freezer$condition(~ input$freezer == "-80")
    
    iv_freezer$add_rule("rack", sv_required())
    
    
    iv$add_validator(iv_freezer)
    
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
      
      list_dta
      
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
    
    
    observeEvent(input$freezer,{
      
      shinyjs::toggleState("rack",  condition = input$freezer == "-80")
    }, ignoreInit = TRUE)
    
    
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
