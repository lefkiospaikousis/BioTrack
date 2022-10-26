#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  
  # Set Up Reactive Values ----
  rv <- reactiveValues(
    
    submission = NULL,
    processed_submission = NULL,
    db_trigger = NULL,
    
  )
  
  added_sample_info <- reactiveVal(FALSE)
  added_specimens <- reactiveVal(FALSE)
  
  session$userData$db_trigger <- reactiveVal(0)
  
  
  # Logic -----
  
  
  mod_storage_information_server("storage_information_1", reactive(rv$processed_submission))
  
  
  res_sample_info <- mod_sample_information_server("sample_information_1")
  
  
  # output$res_sample_info <- renderPrint({
  #   
  #   res_sample_info$dta()
  #   
  # })
  
  observeEvent(res_sample_info$submit(), {
    
    rv$submission <- res_sample_info$dta()
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(rv$submission, {
    
    removeModal()
    
    show_waiter("Processing.. Please wait!", sleep = 0.5)
    
    ## SAVE to DB
    tryCatch({
      
      submission <- process_submission(rv$submission)
      
      submission$specimens <- FALSE
      
      res_save <- DBI::dbAppendTable(dbase_specimen, "sample_info", as.data.frame(submission))
      
      cat("Saved ", res_save, " form\n")
      
      rv$db_trigger <- rv$db_trigger + 1
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      
      if(!golem::app_prod()) showNotification("Saved to Database!")
      
      rv$processed_submission <- submission
      
      added_sample_info(TRUE)
      
    }, error = function(e){
      
      print(e)
      message("Error when saving submission on Database")
      
      # TODO Save submission locally in a dataframe?
      # Inform me?
      
      try({
        saveRDS(rv$submission, paste0("failed_submission", file_time(), ".rds"))
      })
      
    }, finally = hide_waiter())
    
  })
  
  
  observeEvent(added_sample_info(), {
    
    if(isTRUE(added_sample_info())) {
      
    updateTabsetPanel(session, inputId = "tabs", selected = "Specimen")
      
    }
    
  }, ignoreInit = FALSE)
  
}
