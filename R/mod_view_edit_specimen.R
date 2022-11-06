#' view_edit_specimen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_view_edit_specimen_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        textInput(ns("lab_no"), "Specimen Lab No"), 
        uiOutput(ns("lab_noUI"))
      ))
  )
}

#' view_edit_specimen Server Functions
#'
#' @noRd 
mod_view_edit_specimen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      specimen_selected = NULL
    )
    
    
    observe({
      # To set he focus on the view tab for scanning the LAB NO
      #if(input$left_tabs == "view"){
      
      session$sendCustomMessage("selectText", list(NULL))
      #} 
    })
    
    
    observeEvent(input$lab_no, {
      
      req(input$lab_no, cancelOutput = TRUE)
      req(nchar(input$lab_no) == 9, cancelOutput = TRUE)
      
      specimen <- dbase_specimen %>% 
        tbl("specimen_info") %>% 
        filter(lab_no == !!input$lab_no) %>% 
        collect()
      
      if(nrow(specimen) == 0){
        
        validate(glue::glue("Lab no '{input$lab_no}' was not identified"))
        shinyjs::reset("lab_no")
        
      }
      
      shinyjs::reset("lab_no")
      
      rv$specimen_selected <- specimen
      
      
    })
    
    output$lab_noUI <- renderUI({
      
      
      
      req(rv$specimen_selected , cancelOutput = TRUE)
      
      session$userData$db_trigger()
      
      # get it from the database, so that this UI will reflect
      # any changes in the DB
      specimen <- dbase_specimen %>% 
        tbl("specimen_info") %>% 
        filter(unique_id == !!rv$specimen_selected$unique_id) %>% 
        collect()
        
      #specimen <- rv$specimen_selected
      if(nrow(specimen) == 0) validate("Oups! Something went wrong. Contact support!")
      
      
      box(width = NULL,
          tagList(
            h3("Specimen Information"),
            p("Lab no: ", strong(specimen$lab_no)),
            p("Quality of Sample: ", strong(specimen$quality)),
            p("Specimen Type: ", strong(specimen$specimen_type)),
            p(span("Freezer: ", strong(specimen$freezer) , " - Position: ", strong(specimen$place))),
            p("Date processing: ", strong(to_date_time(specimen$date_processing) %>% format("%d/%m/%Y %H:%M"))),
            p("Duration from Receipt to Processing: ", strong(specimen$duration)),
            p("Number of tubes: ", strong(specimen$n_tubes), 
              actionButton(ns("edit_tubes"), "Edit", icon("pen-to-square"), 
                           style = "background-color: transparent; border:none;color:green;margin-bottom:3px")
            )
          )  
      )
      
    })
    
    
    
    observeEvent(input$edit_tubes, {
      
      showModal(
        modalDialog(
          title = "Alter Specimen Information",
          size = "s", footer = NULL,
          mod_modify_specimen_ui(ns("n_tubes"))
        )
      )
      
    })
    
    res <- mod_modify_specimen_server("n_tubes", reactive(rv$specimen_selected))
    
    
    observeEvent(res$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    observeEvent(res$submit(), {
      
      new_value <- res$new_value()
      col <- res$id
      
      id <- rv$specimen_selected$unique_id
      
      x <- glue::glue_sql("UPDATE specimen_info SET {col} = {new_value} WHERE unique_id = {id}", .con = dbase_specimen)
      
      rs <- DBI::dbExecute(dbase_specimen, x)
      
      if(rs == 1){
        
        cat("Updated ", col, "for ", rs, " specimen_info for id = ", id, "\n")
          
      } else {
        
        cat("Failed to update the ", col, " for specimen wih id ", id, "\n")
        
      }
      
      if(!golem::app_prod()) showNotification(
        glue::glue("Updated specimen ", rv$specimen_selected$lab_no, " - changed {col} to {new_value}")
        )
      
      removeModal()
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      show_toast("success", "", "Successful change")
      
      
    }, ignoreInit = TRUE)
    
  })
}

## To be copied in the UI
# mod_view_edit_specimen_ui("view_edit_specimen_1")

## To be copied in the server
# mod_view_edit_specimen_server("view_edit_specimen_1")
