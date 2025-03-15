#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  shiny::fluidPage(
    
    theme = 
      bslib::bs_theme(
        version = 5,
        bg = "#ededeb",
        fg = "#193244",
        primary = "#193244",
        heading_font = bslib::font_google("Montserrat", wght = 800),
        base_font = bslib::font_google("Montserrat", wght = 600),
        ) %>% 
      bslib::bs_add_variables(
        "font-size-base" = "0.65rem",
        "h1-font-size" = "1.375rem",   
        "h2-font-size" = "1.25rem",
        "h3-font-size" = "1rem"
      ) %>% 
      bslib::bs_add_rules("
        .nav-pills .nav-link {
           border-radius: 0px !important;
        }
      "),
    
    golem_add_external_resources(), ## Keep this for now
    
    shiny::div(
      style = "display: flex; align-items: center; 
               gap: 20px; background-color: #ededeb;
               border-bottom: 0.5px solid #193244;", 
      shiny::img(),
      
      shiny::h1("StructuredEdge", style = "margin: 0;"),
      
      shiny::tabsetPanel(
        id = "main_tabs",
        type = "pills",
        shiny::tabPanel("Creation Zone", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1")),
        shiny::tabPanel("Product Payoff", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1")),
        shiny::tabPanel("Market Data", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1")),
        shiny::tabPanel("Development", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "StructuredEdge"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
