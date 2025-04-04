#' dev UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dev_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dev Server Functions
#'
#' @noRd 
mod_dev_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_dev_ui("dev_1")
    
## To be copied in the server
# mod_dev_server("dev_1")
