#' productPayoff UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_productPayoff_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' productPayoff Server Functions
#'
#' @noRd 
mod_productPayoff_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_productPayoff_ui("productPayoff_1")
    
## To be copied in the server
# mod_productPayoff_server("productPayoff_1")
