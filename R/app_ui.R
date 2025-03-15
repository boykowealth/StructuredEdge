#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  shiny::fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "yeti",
                            heading_font = bslib::font_google("Montserrat"),
                            base_font = bslib::font_google("Montserrat"),
                            ),
    
    golem_add_external_resources(), ## Keep this for now
    
    titlePanel("StructuredEdge"),
    
    ## Create Multiple Page Tab Selection
    tags$head(
      tags$style(HTML("
      .nav-tabs {
        display: flex;
        flex-direction: column;
        width: 150px;
      }
      .tab-content {
        margin-left: 170px;
      }
    "))
    ),
    shiny::navlistPanel(
      id = "vertical_tabs",
      widths = c(2, 10),
      shiny::tabPanel("Creation Zone", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1")),
      shiny::tabPanel("Product Payoff", structuredProductPortfolio_ui("structuredProductPortfolio_ui_1")),
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
