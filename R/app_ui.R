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
        base_font = bslib::font_google("Montserrat", wght = 600)
      ) %>% 
      bslib::bs_add_variables(
        "font-size-base" = "0.65rem",
        "h1-font-size" = "1.375rem",   
        "h2-font-size" = "1.25rem",
        "h3-font-size" = "1rem"
      ) %>% 
      bslib::bs_add_rules("
      
        .nav-pills .nav-link {
           border-radius: 3px !important;
        }
        
        .card.bslib-card.bslib-mb-spacing.html-fill-item.html-fill-container, .bslib-card.bslib-mb-spacing.html-fill-item.html-fill-container.well {
          box-shadow: 0px px 0px rgba(0, 0, 0, 0.1);
          border-radius: 3px !important;
          border-width: 0.25px;
          border-color: #193244;
          border-style: solid;
        }
        
      "),
    
    golem_add_external_resources(), ## Keep this for now
    
    ## BRANDING <START>
    shiny::div(
      style = "display: flex; align-items: center; 
               gap: 20px; background-color: #ededeb;
               border-bottom: 0.5px solid #193244;", 
      shiny::tags$img(
        src = "https://raw.githubusercontent.com/boykowealth/StructuredEdge/refs/heads/main/graphics/BW_LOGO_BLUE.png",
        alt = "StructuredEdge Logo",
        style = "width: 100%; max-width: 40px;"
      ),
      shiny::h1("StructuredEdge", style = "margin: 0;")
    ),
    ## BRANDING <END>
    
    ## APP MODULES <START>
    shiny::div(
      style = "margin-top: 5px; margin-bottom: 20px;",
      shiny::tabsetPanel(
        id = "main_tabs",
        type = "pills",
        shiny::tabPanel("Creation Zone", mod_creationZone_ui("creationZone_ui_1")),
        shiny::tabPanel("Product Payoff", mod_productPayoff_ui("productPayoff_ui_1")),
        shiny::tabPanel("Market Simulation", mod_productSim_ui("productSim_ui_1")),
        shiny::tabPanel("Development", mod_dev_ui("dev_ui_1"))
      )
    )
    ## APP MODULES <END>
    
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
