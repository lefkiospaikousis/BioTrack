#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  res_auth <- shinymanager::secure_server(
    
    check_credentials = shinymanager::check_credentials(
      db = get_golem_config("db_users"),
      
      passphrase = get_golem_config("users_passphrase")
    ),
    keep_token = TRUE
  )
  
  # Set Up Reactive Values ----
  rv <- reactiveValues(
    
    focus = 0 # I use it as a trigger to focus the input$lab_no in tab View/Edit Specimen
    
  )
  
  session$userData$db_trigger <- reactiveVal(0)
  
  observe({
    session$userData$user <- res_auth$user #"lefkios"#
    session$userData$user_info <- reactiveValuesToList(res_auth)
    #session$userData$user_info$admin = TRUE
  })
  output$userName <- renderText({
    
    paste0("User: ", session$userData$user)
    
  })
  
  observeEvent(input$left_tabs, {
    
    if(input$left_tabs == "view") {
      rv$focus = rv$focus + 1
    }
  })
  
  mod_view_edit_specimen_server("view_edit_specimen_1", reactive(rv$focus)) 
  mod_tables_server("tables_1", tbl_merged)
  mod_log_file_server("log_file_1")
  mod_statistics_server("statistics_1")
  # # Step 2 -Add specimens and storage information
  # observeEvent(added_sample_info(), {
  #   
  #   if(isTRUE(added_sample_info())) {
  #     updateTabsetPanel(session, inputId = "tabs", selected = "Specimen")
  #     added_sample_info(FALSE)
  #   }
  #   
  # }, ignoreInit = TRUE)
  
  # END. Go back to Step 1
  # observeEvent(added_specimens(),{
  #   
  #   waiter::waiter_update(html = html_waiter("Initialising. Please wait..."))
  #   Sys.sleep(1)
  #   shinyjs::refresh()
  #   
  #   
  # }, ignoreInit = TRUE)
  
  
  # Adding sample Information -----
  
  res_sample_info <- mod_sample_information_server("sample_information_1")
  
  
  observeEvent(res_sample_info$submit(), {
    
    show_waiter("Processing.. Please wait!", sleep = 0.5)
    
    ## Update sample_info to DB
    tryCatch({
      
      the_samples <- reactiveValuesToList( res_sample_info$samples )
      sample_info <- res_sample_info$dta()
      
      # save the icf file to the correct location. Pick any id form the ones form the sample
      an_id <- the_samples[[1]][["unique_id"]]
      dir.create(path_dir <- file.path("ICF", an_id))
      new_path <- file.path(path_dir, res_sample_info$icf_path()$name)
      res <- file.copy(res_sample_info$icf_path()$datapath, new_path, overwrite = TRUE)
      sample_info$path_icf <- basename(new_path) %>% paste(collapse = "\n")
      
      if(isFALSE(res)){
        
        show_toast("warning", "Oups..!", 
                   "The sample information was succesfully stored. However, the 
                   we could not save the 'icf' pdf file. Please contact support",
                   keepVisible = TRUE
        )
        
        sample_info$path_icf <- NA_character_
      }
      
      # Add all the patient information to all their sample
      
      updated <- purrr::map_int(names(the_samples),  function(id){
        
        
        sql_cmd <- glue::glue_sql("UPDATE sample_info SET 
                                  firstname = {sample_info$firstname}, 
                                  surname = {sample_info$surname}, 
                                  gender = {sample_info$gender}, 
                                  dob = {as.numeric(sample_info$dob)}, 
                                  nationality = {sample_info$nationality}, 
                                  diagnosis = {sample_info$diagnosis}, 
                                  status = {sample_info$status}, 
                                  doctor = {sample_info$doctor}, 
                                  consent = {sample_info$consent}, 
                                  civil_id = {sample_info$civil_id}, 
                                  study_id = {sample_info$study_id}, 
                                  study = {sample_info$study}, 
                                  comments = {sample_info$comments}, 
                                  path_icf = {sample_info$path_icf}
                                  WHERE unique_id = {id}", .con = dbase_specimen)
        
        rs <- DBI::dbExecute(dbase_specimen, sql_cmd)
        
        rs
        
      })
      
      stopifnot({ all(as.logical(updated)) })
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      
      # Add to log. Only the patient info
      try({add_to_logFile("Finalised Sample Information Form", session$userData$user, info = sample_info)}, silent = FALSE)
      
      waiter::waiter_update(html = html_waiter("Succesfull submission of Sample Information"))
      Sys.sleep(2)
      waiter::waiter_update(html = html_waiter("Initialising. Please wait..."))
      Sys.sleep(1)
      shinyjs::refresh()
      
    }, error = function(e){
      
      print(e)
      cat("Error when saving sample_info on Database\n")
      
      # TODO Save submission locally in a dataframe? Inform me?
      
      shinyFeedback::showToast("error", title = "Error while saving the Sample Information!",
                               keepVisible = TRUE, .options = list(positionClass = "toast-top-center"),
                               
                               "Could not save this Sample Information to Database! Check your network connectivity and try again.
                                 If the problem persists, please contact support")
      
      try({
        saveRDS(sample_info, paste0("failed_submission", file_time(), ".rds"))
      })
      
    }, finally = {
      
      hide_waiter()
    })
    
  }, ignoreInit = TRUE)
  
  
  mod_freezer_log_server("freezer_log_1", tbl_merged)
  
  # Dbase Tables ----
  
  tbl_registry <- reactive({
    session$userData$db_trigger()
    dbase_specimen %>% 
      tbl("sample_info") %>% 
      collect() 
    
  })
  
  tbl_specimen <- reactive({
    session$userData$db_trigger()
    dbase_specimen %>% 
      tbl("specimen_info") %>% 
      collect()
    
  })
  
  
  tbl_merged <- reactive({
    #session$userData$db_trigger()
    merged <- left_join(tbl_registry(), tbl_specimen(), by = "unique_id") 
    
    merged %>% 
      filter(unique_id != "testid") %>% 
      mutate(
        date_receipt = to_date_time(date_receipt),
        date_processing = to_date_time(date_processing),
        date_collection = to_date_time(date_collection),
        dob = to_date(dob),
        date_shipment = to_date(date_shipment)
      )
  })
  
  
}
