#' statistics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_statistics_ui <- function(id){
  ns <- NS(id)
  
  years <- seq("2023", to = lubridate::year(Sys.Date()), by = 1)
  
  
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(ns("year"), "Collection year", choices = c("ALL", years)),
                   selectInput(ns("study"), "Study", choices = NULL),
                   selectInput(ns("phase"), "Phase", choices = NULL),
                   br(),
                   actionButton(ns("get_report"), "Get report", width = "100%", class = "btn-go"),
                   br(),
                   br(),
                   downloadLink(ns("down_docx"), "Download as .docx"),
                   br(),
                   br(),
                   downloadLink(ns("down_xlsx"), "Download as .xlsx")
      ),
      mainPanel(width = 8,
                box(width = NULL,
                    uiOutput(ns("tbl_flex"))
                )
      )
    )
  )
}

#' statistics Server Functions
#'
#' @noRd 
mod_statistics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    w <- waiter::Waiter$new(id = ns("tbl_flex"))
    
    #page_props <- officer::prop_section(page_size = officer::page_size(orient = "landscape"))
    
    diagnosis_types <- c("Colon", "Lung", "Breast")
    
    
    lazy_table <- reactive({
      
      req(input$year)
      
      lazy_tbl <- dbase_specimen |>  tbl("sample_info") 
      
      if(input$year != "ALL"){
        
        first_date <- lubridate::ymd_hm(paste0(input$year, "0101-00:00")) |> as.numeric()
        last_date <- lubridate::ymd_hm(paste0(input$year, "1231-23:59")) |> as.numeric()
        
        lazy_tbl <- lazy_tbl |> 
          filter(date_collection >= first_date & date_collection <= last_date)
        
      }
      
      lazy_tbl
      
    })
    
    params <- reactive({
      
      lazy_table() |> 
        distinct(study, phase) |> 
        collect()
      
    })
    
    
    observeEvent(params(), {
      
      studies <- unique(params()$study)
      
      updateSelectInput(session, "study", choices = studies)
    })
    
    observeEvent(input$study, {
      
      #freezeReactiveValue(input, "phase")
      
      phases <- params() |> filter(study == !!input$study) |> distinct(phase) |> pull()
      
      updateSelectInput(session, "phase", choices = phases, selected = character(0))
      
    })
    
    
    tbl_stats <- reactive({
      
      req(input$study)
      req(input$phase)
      req(input$year)
      #session$userData$db_trigger()
      w$show()
      
      samples <- lazy_table() |> 
        filter(study == !!input$study) |> 
        filter(phase == !!input$phase) |> 
        collect() |> 
        mutate(study = toupper(study))
      
      if(nrow(samples) == 0 ) {
        validate(glue::glue("No samples found for Study: {input$study} at Phase: {input$phase}"))
      }
      
      tbl_counts <- samples |> 
        select(bococ, diagnosis, date_collection) |> 
        distinct(bococ, .keep_all = TRUE) |> 
        mutate(
          date_collection = purrr::map(date_collection, to_date_time)
        ) |> 
        tidyr::unnest_longer(c(date_collection)) |> 
        mutate(date_collection = as.Date(date_collection),
               month = lubridate::month(date_collection) |> stringr::str_pad(2, pad = "0"),
               year = lubridate::isoyear(date_collection),
               isoweek = lubridate::isoweek(date_collection),
               year_month = paste0(year,month)
        ) |> 
        count(year_month, isoweek, diagnosis) |> 
        group_by(year_month, diagnosis) |> 
        mutate(week = row_number() )|> 
        select(-isoweek) |> 
        tidyr::complete(week = c(1:5), fill = list( n = 0) ) |> 
        ungroup()
      
      ttl_week <- tbl_counts |> 
        summarise(n = sum(n), .by = c(year_month, diagnosis)) |> 
        mutate(week = "Total")
      
      ttl_diagnosis <- tbl_counts |> 
        summarise(n = sum(n), .by = c(year_month, week)) |> 
        mutate(diagnosis = "Total") |> 
        mutate(week = as.character(week))
      
      tbl_totals <- tbl_counts |> 
        mutate(week = as.character(week)) |> 
        bind_rows(ttl_week) |> 
        bind_rows(ttl_diagnosis) |> 
        arrange(year_month, diagnosis, week) |> 
        tidyr::pivot_wider(names_from = diagnosis, values_from = n, values_fill = 0) |> 
        relocate(Total, .after = last_col()) |> 
        mutate(
          year_month = lubridate::ym(year_month) |> format("%b-%Y")
        ) |> 
        rename(
          Month = year_month,
          Week = week
        )
      
    }) |> 
      bindEvent(input$get_report)
    
    
    tbl_flex <- reactive({
      
      req(tbl_stats())
      
      rep_study <- isolate(input$study) 
      rep_phase <- isolate( input$phase ) # dont depend on this, only on tbl_stats that is binded to get Report
      rep_header <- glue::glue("Recruited patients at {rep_phase}")
      rep_title <- glue::glue("Study: {rep_study}")
      n_col <- ncol(tbl_stats())
      
      tbl_stats() |> 
        flextable::flextable() |> 
        flextable::theme_box() |> 
        flextable::merge_v(j = "Month") |> 
        flextable::add_header_row(values = c("", rep_header), colwidths = c(2, n_col - 2)) |> 
        flextable::set_caption(
          flextable::as_paragraph(flextable::as_chunk(rep_title, 
                                                      props = officer::fp_text(bold = TRUE, font.size = 14))),
          fp_p = officer::fp_par(padding.bottom = 10)
        )
    })
    
    output$tbl_flex <- renderUI({
      
      tbl_flex() |> 
        flextable::htmltools_value()
      
    })
    
    output$down_xlsx <- downloadHandler(
      
      filename = function(){
        "statistics.xlsx"
      },
      
      content = function(file) {
        
        #x <- list(tbl_stats())
        #names(x) <- paste0("Study_", input$study, "-", input$phase) |> clean_string()
        readr::write_excel_csv(
          tbl_stats(),
          file = file
        )
      }
    )
    
    output$down_docx <- downloadHandler(
      
      filename = function(){
        #glue::glue("Freezer_{input$freezer}{rv$rack_text}-{format(Sys.time(), '%d/%m/%Y-%H_%M')}.html")
        # for some weird reason, the above filename creates issues when the shiny runs on the shiny server
        # a the shiny user. However, when I run the app thought a user, then no problems...WTF
        "statistics.docx"
      },
      
      content = function(file) {
        
        tbl_flex() %>% 
          flextable::add_footer_lines(glue::glue("Date exported: ", format(Sys.time(), "%d/%m/%Y %H:%M"))) %>% 
          flextable::save_as_docx(
            path = file
            #pr_section = page_props
          )
      }
    )
    
  })
}

## To be copied in the UI
# mod_statistics_ui("statistics_1")

## To be copied in the server
# mod_statistics_server("statistics_1")
