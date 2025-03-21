#' The application server-side
#'un
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  r <- shiny::reactiveValues()
  
  mod_creationZone_server("creationZone_ui_1", r = r)
  mod_productPayoff_server("productPayoff_ui_1", r = r)
  
}

