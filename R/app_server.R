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
    db_trigger = NULL
    
    
  )
  
  added_sample_info <- reactiveVal(FALSE)
  added_specimens <- reactiveVal(FALSE)
  
  session$userData$db_trigger <- reactiveVal(0)
  
  mod_view_edit_specimen_server("view_edit_specimen_1")  
  # Switching between tabs ----
  
  # Step 1 - Add Sample Information
  # observeEvent(input$add_sample_info, {
  #   
  #   updateTabsetPanel(session, inputId = "tabs", selected = "Add sample info")
  #   
  # })
  
  # Step 2 -Add specimens and storage information
  observeEvent(added_sample_info(), {
    
    if(isTRUE(added_sample_info())) {
      updateTabsetPanel(session, inputId = "tabs", selected = "Specimen")
      added_sample_info(FALSE)
    }
    
  }, ignoreInit = TRUE)
  
  # END. Go back to Step 1
  observeEvent(added_specimens(),{
    
    show_waiter("Initialising. Please wait...", sleep = 3)
    # I have split the process in 3 pages
    # when I move from page to page,the form details of each page stays there
    # Need to reload to be cleared
    # TODO use shinjs::reset() ?
    session$reload()
    
    
  }, ignoreInit = TRUE)
  
  
  # Adding sample Information -----
  
  
  res_sample_info <- mod_sample_information_server("sample_information_1")
  # Returns: dta(), submit(), cancel()
  
  observeEvent(res_sample_info$cancel(), {
    
    updateTabsetPanel(session, inputId = "tabs", selected = "Add sample info")
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(res_sample_info$submit(), {
    
    rv$sample_info <- res_sample_info$dta()
    rv$sample_info$path_icf <- res_sample_info$icf_path()
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(rv$sample_info, {
    
    removeModal()
    
    show_waiter("Processing.. Please wait!", sleep = 0.5)
    
    ## SAVE to DB
    tryCatch({
      
      sample_info <- process_submission(rv$sample_info)
      
      # save the icf file to the correct location
      dir.create(path_dir <- file.path("ICF", sample_info$unique_id))
      new_path <- file.path(path_dir, "ICF.pdf")
      
      
      res <- file.copy(sample_info$path_icf, new_path)
      sample_info$path_icf <- new_path
      
      if(isFALSE(res)){
        
        show_toast("warning", "Oups..!", 
                   "The sample information was succesfully stored. However, the 
                   we could not save the 'icf' pdf file. Please contact support",
                   keepVisible = TRUE
        )
        
        sample_info$path_icf <- NA_character_
      }
      
      
      res_save <- DBI::dbAppendTable(dbase_specimen, "sample_info", as.data.frame(sample_info))
      
      cat("Saved ", res_save, " form\n")
      
      if(!golem::app_prod()) showNotification("Saved to Database!")
      
      rv$processed_sample_info <- sample_info
      
      waiter::waiter_update(html = html_waiter("Moving to Storage information"))
      Sys.sleep(1)      
      added_sample_info(TRUE)
      
      #rv$db_trigger <- rv$db_trigger + 1
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      
    }, error = function(e){
      
      print(e)
      cat("Error when saving sample_info on Database\n")
      
      # TODO Save submission locally in a dataframe?
      # Inform me?
      
      shinyFeedback::showToast("error", title = "Error while saving the Sample Information!",
                               keepVisible = TRUE, .options = list(positionClass = "toast-top-center"),
                               
                               "Could not save this Sample Information to Database! Check your network connectivity and try again.
                                 If the problem persists, please contact support")
      
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
