#' edit_sample_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_edit_sample_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("edit"), "", icon("pen-to-square"), class = "btn_edit")
  )
}

#' edit_sample_button Server Functions
#'
#' @noRd 
mod_edit_sample_button_server <- function(id, sample_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    observeEvent(input$edit, {
      
      ids_allowed_all <- c("n_tubes", "status")
      
      if(isTRUE(as.logical(session$userData$user_info$admin)) | id %in% ids_allowed_all) {
        
        showModal(
          modalDialog(
            title = "Editing Sample Information data",
            size = "s", footer = NULL,
            mod_modal_modifyValue_ui(ns(id))
          )
        )
        
      } else {
        
        show_toast("error", "You are not authorised to change this field",
                   "An administrator can change this field")
      }
      
      
    })
    
    res <- mod_modal_modifyValue_server(id, sample_info)
    
    
    observeEvent(res$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    observeEvent(res$submit(), {
 
      new_value <- res$new_value()
      col <- res$id
      
      unique_id <- sample_info()$unique_id
      
      
      if(res$id %in% date_time_cols) {
        new_value <-  as.numeric(lubridate::dmy_hm(new_value, tz = "EET"))
      }
      
      
      if(res$id %in% date_cols | lubridate::is.Date(new_value)){
        new_value <-  as.numeric(new_value)
      }
      
      if(res$id == "bococ"){
        new_value <- stringr::str_pad(new_value, 6, 'left', '0')
      }
      
      sql_cmd <- glue::glue_sql("UPDATE sample_info SET {col} = {new_value} WHERE unique_id = {unique_id}", .con = dbase_specimen)
      
      rs <- DBI::dbExecute(dbase_specimen, sql_cmd)
      
      
      # need to update the duration if one of these are changed
      if(res$id %in% c("date_collection", "date_processing")){
        
        # get the updated sample Info, after I changed the value fo date_collection
        sample_info <- get_sample_info(dbase_specimen, unique_id)
        #sample_info <- as.list(sample_info())
        sample_info$date_processing <- get_fromDB(dbase_specimen, "specimen_info", "date_processing", unique_id)
        sample_info$duration <- get_fromDB(dbase_specimen, "specimen_info", "duration", unique_id)
        
        # this might be more than one. Update_duration() handles that
        sample_info$lab_no <- get_fromDB(dbase_specimen, "specimen_info", "lab_no", unique_id)
        
        tryCatch({
          
          update_duration(dbase_specimen, sample_info, session$userData$user)
          rm(sample_info)
          
        }, error = function(e){
          show_toast("warning", "Duration from collection to processing", 
                     "Could not update the Duration from Sample Collection to Sample Processing.
                    Please contact support to make the change", keepVisible = TRUE)
        })
        #}
      }
      
      
      if(rs == 1){ 
        
        cat("Updated ", col, "for ", rs, " sample_info with unique_id: ", unique_id, "\n")
        
      } else {
        
        cat("Failed to update the ", col, " sample_info with unique_id: ", unique_id, "\n")
        
      }
      
      #if(!golem::app_prod()) 
      showNotification(
        glue::glue("Updated sample_info ", unique_id, " - changed {col} to {new_value}")
      )
      
      removeModal()
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      show_toast("success", "", "Successful change")
      
      # Add to log
      try({add_to_logFile("Modified Sample Information data", session$userData$user, 
                          info = list(lab_no = "", #sample_info()$lab_no,
                                      bococ = sample_info()$bococ,
                                      col = col,
                                      old_value = sample_info()[[col]],
                                      new_value = new_value
                                      
                          ))}, silent = TRUE)
      
      
    }, ignoreInit = TRUE)
    
  })
}

## To be copied in the UI
# mod_edit_sample_button_ui("edit_sample_button_1")

## To be copied in the server
# mod_edit_sample_button_server("edit_sample_button_1")
