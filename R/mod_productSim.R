#' productSim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_productSim_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' productSim Server Functions
#'
#' @noRd 
mod_productSim_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_productSim_ui("productSim_1")
    
## To be copied in the server
# mod_productSim_server("productSim_1")
