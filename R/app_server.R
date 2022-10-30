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
    
    sample_info = NULL,
    processed_sample_info = NULL,
    db_trigger = NULL,
    
  )
  
  added_sample_info <- reactiveVal(FALSE)
  added_specimens <- reactiveVal(FALSE)
  
  session$userData$db_trigger <- reactiveVal(0)
  
  
  # Switching between tabs ----
  
  # Step 1 - Add Sample Information
  observeEvent(input$add_sample_info, {
    
    updateTabsetPanel(session, inputId = "tabs", selected = "Add sample info")
    
  })
  
  # Step 2 -Add specimens and storage information
  observeEvent(added_sample_info(), {
    
    if(isTRUE(added_sample_info())) {
      updateTabsetPanel(session, inputId = "tabs", selected = "Specimen")
      added_sample_info(FALSE)
    }
    
  }, ignoreInit = TRUE)
  
  # END. Go back to Step 1
  observeEvent(added_specimens(),{
   
    # Update the sample_info with number of specimens
    dta <- res_storage_info$dta()
    id <- dta$unique_id[1]
    n_specimens <- nrow(dta)
    
    x <- glue::glue_sql("UPDATE sample_info SET specimens = {n_specimens} WHERE unique_id = {id}", .con = dbase_specimen)
    
    rs <- DBI::dbExecute(dbase_specimen, x)
    
    cat("Updated Number of specimens for ", rs, " sample_informaton\n
        for id = ", id, "\n")
    
    if(!golem::app_prod()) showNotification("Updated sample information with number of specimens")
    # I have split the process in 3 pages
    # when I move from page to page,the form detials of each page stays there
    # Need to reload to vbe cleared
    # TODO use shinjs::reset() ?
    
    show_waiter("Saving the infomation.. Please wait", sleep = 0.5)
    session$reload()
    
  }, ignoreInit = TRUE)
  
  
  # Adding sample Information -----
  
  
  res_sample_info <- mod_sample_information_server("sample_information_1")
  # Returns: dta(), submit(), cancel()
  
  observeEvent(res_sample_info$cancel(), {
    
    updateTabsetPanel(session, inputId = "tabs", selected = "Initial")

  }, ignoreInit = TRUE)
  
  
  observeEvent(res_sample_info$submit(), {
    
    rv$sample_info <- res_sample_info$dta()
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(rv$sample_info, {
    
    removeModal()
    
    show_waiter("Processing.. Please wait!", sleep = 0.5)
    
    ## SAVE to DB
    tryCatch({
      
      sample_info <- process_submission(rv$sample_info)
      
      sample_info$specimens <- FALSE
      
      res_save <- DBI::dbAppendTable(dbase_specimen, "sample_info", as.data.frame(sample_info))
      
      cat("Saved ", res_save, " form\n")
      
      rv$db_trigger <- rv$db_trigger + 1
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      
      if(!golem::app_prod()) showNotification("Saved to Database!")
      
      rv$processed_sample_info <- sample_info
      
      added_sample_info(TRUE)
      
    }, error = function(e){
      
      print(e)
      message("Error when saving sample_info on Database")
      
      # TODO Save submission locally in a dataframe?
      # Inform me?
      
      try({
        saveRDS(rv$sample_info, paste0("failed_submission", file_time(), ".rds"))
      })
      
    }, finally = hide_waiter())
    
  })
  
  # Adding specimen and storage information ----
  
  res_storage_info <- mod_storage_information_server("storage_information_1", reactive(rv$processed_sample_info))

  observeEvent(res_storage_info$submit(), {

    added_specimens(TRUE)
    
  }, ignoreInit = TRUE)
  
}
