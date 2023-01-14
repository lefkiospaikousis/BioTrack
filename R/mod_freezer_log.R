#' freezer_log UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_freezer_log_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(ns("freezer"), "Freezer", choices = c(freezer_20, freezers_80)),
                   selectInput(ns("rack"),"Rack", choices = NULL),
                   downloadLink(ns("down_freezer_log"), "Download Log as .html")
      ),
      mainPanel(width = 8,
                box(width = NULL,
                    uiOutput(ns("spec_log"))
                )
      )
    )
    
  )
}

#' freezer_log Server Functions
#'
#' @noRd 
mod_freezer_log_server <- function(id, tbl_merged){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
    
    rv <- reactiveValues(rack_text = NULL)
    
    observeEvent(input$freezer, {
      req(input$freezer)
      
      choices <- freezer_internals(input$freezer)$rack 
      updateSelectInput(session, "rack", choices = choices)
      
      shinyjs::toggleState("rack", condition = input$freezer %in% freezers_80)
      
    })
    
    observeEvent(input$rack, {
      
      rv$rack_text <- 
        if(input$freezer %in% freezers_80) {
          paste0("RACK ", input$rack)
        } else {
          # other freezers do not have racks
          ""
          
        }
      
    })
    
    output$down_freezer_log <- downloadHandler(
      
      filename = function(){
        #glue::glue("Freezer_{input$freezer}{rv$rack_text}-{format(Sys.time(), '%d/%m/%Y-%H_%M')}.html")
        # for some weird reason, the above filename creates issues when the shiny runs on the shiny server
        # a the shiny user. However, when I run the app thought a user, then no problems...WTF
        "storage_log.docx"
      },
      
      content = function(file) {
        
        spec_log() %>% 
          flextable::add_footer_lines(glue::glue("Date exported: ", format(Sys.time(), "%d/%m/%Y %H:%M"))) %>% 
          #flextable::save_as_html(path = file)
          flextable::save_as_docx(
            path = file,
            pr_section = page_props
          )
        
        
      }
      
      
    )
    
    
    
    output$spec_log <- renderUI({
      
      spec_log() %>%
        flextable::htmltools_value()
      
    })
    
    spec_log <- reactive({
      
      drawers <- freezer_internals(input$freezer)$drawer
      boxes <-  freezer_internals(input$freezer)$box
      
      if(input$freezer %in% freezers_80){
        
        log_version <- get_golem_config("log_version80") 
        title <- glue::glue("{input$freezer} Freezer - {rv$rack_text} - Specimen Log")
        
        rack_tbl <- tbl_merged() %>% 
          select(lab_no, freezer, box, rack, drawer, unique_id) %>% 
          filter(freezer == !!input$freezer, rack == !!input$rack) %>% 
          select(drawer, box, lab_no) %>% 
          tidyr::complete(box = as.character(boxes), drawer = as.character(drawers)) %>%  
          rename (Box = box) %>% 
          tidyr::pivot_wider( 
            names_from = drawer, 
            values_from = lab_no, 
            values_fn = ~glue::glue_collapse(., sep = "\n")
          ) 
        
        
        out <- rack_tbl %>% 
          flextable::flextable() %>% 
          flextable::add_header_row(values = c(rv$rack_text, "Drawers"), colwidths = c(1, length(drawers))) %>% 
          flextable::theme_box() %>%
          flextable::set_caption(
            flextable::as_paragraph(flextable::as_chunk(title, 
                                                        props = officer::fp_text(bold = TRUE, font.size = 14))),
            fp_p = officer::fp_par(padding.bottom = 10)
          ) %>% 
          flextable::set_table_properties("fixed") %>% 
          flextable::width( j = c(2:(length(drawers)+1)), width = 1.80) %>% 
          flextable::fontsize(size = 9) %>% 
          flextable::add_footer_lines(glue::glue("BOCOC: {input$freezer} Freezer. {rv$rack_text}. Specimen {log_version}")) 
        
        
        return(out)
        
      } 
      
      
      if(input$freezer == freezer_20){
        
        # Freezer -20, does not have Racks nor Boxes. Just Drawers
        
        title <- glue::glue("{input$freezer} Freezer - Specimen Log")
        log_version <- get_golem_config("log_version20") 
        
        rack_tbl <- tbl_merged() %>% 
          select(lab_no, freezer, drawer, unique_id) %>% 
          filter(freezer == !!input$freezer) %>% 
          select(drawer, lab_no) %>% 
          tidyr::complete(drawer = as.character(drawers)) %>%  
          tidyr::pivot_wider( 
            names_from = drawer, 
            values_from = lab_no, 
            values_fn = ~glue::glue_collapse(., sep = "\n")
          ) 
        
        
        out <- rack_tbl %>% 
          flextable::flextable() %>% 
          flextable::add_header_row(values = c("Drawers"), colwidths = length(drawers)) %>% 
          flextable::theme_box() %>%
          flextable::set_caption(
            flextable::as_paragraph(flextable::as_chunk(title, 
                                                        props = officer::fp_text(bold = TRUE, font.size = 14))),
            fp_p = officer::fp_par(padding.bottom = 10)
          ) %>% 
          flextable::set_table_properties("fixed") %>% 
          flextable::width( j = c(1:length(drawers)), width = 1.80) %>% 
          flextable::fontsize(size = 9) %>% 
          flextable::add_footer_lines(glue::glue("BOCOC: {input$freezer} Freezer. Specimen {log_version}"))
        
        return(out)
      }
      
      validate("Unknown type of Freezer")
      
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_freezer_log_ui("freezer_log_1")

## To be copied in the server
# mod_freezer_log_server("freezer_log_1")
