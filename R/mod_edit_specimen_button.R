#' edit_specimen_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_edit_specimen_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("edit"), "", icon("pen-to-square"), class = "btn_edit")
  )
}
    
#' edit_specimen_button Server Functions
#'
#' @noRd 
mod_edit_specimen_button_server <- function(id, specimen){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$edit, {
      
      ids_allowed_all <- c("n_tubes", "status")
      
      if(isTRUE(as.logical(session$userData$user_info$admin)) | id %in% ids_allowed_all) {
        
      showModal(
        modalDialog(
          title = "Editing Specimen Information",
          size = "s", footer = NULL,
          mod_modal_modifyValue_ui(ns(id))
        )
      )
        
      } else {
        
        show_toast("error", "You are not authorised to change this field",
                   "An administrator can change this field")
      }
      
      
    })
    
    res <- mod_modal_modifyValue_server(id, specimen)
    
    
    observeEvent(res$cancel(), {
      
      removeModal()
      
    }, ignoreInit = TRUE)
    
    observeEvent(res$submit(), {
      
      new_value <- res$new_value()
      col <- res$id
      
      lab_no <- specimen()$lab_no
      
      browser()
      if(res$id %in% date_time_cols) {
        new_value <-  as.numeric(lubridate::dmy_hm(new_value, tz = "EET"))
      }
      
      
      if(res$id %in% date_cols | lubridate::is.Date(new_value)){
        new_value <-  as.numeric(new_value)
      }
      
      if(res$id == "bococ"){
        new_value <- stringr::str_pad(new_value, 6, 'left', '0')
      }
      
      sql_cmd <- glue::glue_sql("UPDATE specimen_info SET {col} = {new_value} WHERE lab_no = {lab_no}", .con = dbase_specimen)
      
      rs <- DBI::dbExecute(dbase_specimen, sql_cmd)
      
      

      if(res$id == "specimen_type"){
      # Update also the lab_no
        
        
        new_lab_no <- gsub("[A-Z]{2}$", specimen_types[[new_value]], lab_no)
        
        sql_cmd1 <- glue::glue_sql("UPDATE specimen_info SET lab_no = {new_lab_no} WHERE lab_no = {lab_no}", .con = dbase_specimen)
        
        rs1 <- DBI::dbExecute(dbase_specimen, sql_cmd1)
        
      }
      
      
      if(rs == 1){ 
        
        cat("Updated ", col, "for ", rs, " specimen with lab_no: ", lab_no, "\n")
        
      } else {
        
        cat("Failed to update the ", col, " specimen with lab_no: ", lab_no, "\n")
        
      }
      
      if(!golem::app_prod()) showNotification(
        glue::glue("Updated specimen ", lab_no, " - changed {col} to {new_value}")
      )
      
      removeModal()
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      show_toast("success", "", "Successful change")
      
      bococ <- dbase_specimen %>% tbl("sample_info") %>% filter(unique_id == !!specimen()$unique_id) %>% pull(bococ)
      
      # Add to log
      try({add_to_logFile("Modified specimen info", session$userData$user, 
                          info = list(lab_no = specimen()$lab_no,
                                      bococ = bococ,
                                      col = col,
                                      old_value = specimen()[[col]],
                                      new_value = new_value
                                      
                          ))}, silent = TRUE)
      
      
    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_edit_specimen_button_ui("edit_specimen_button_1")
    
## To be copied in the server
# mod_edit_specimen_button_server("edit_specimen_button_1")
