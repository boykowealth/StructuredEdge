#' Temp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

productPayoff_ui <- function(id){
  ns <- NS(id)
  
}


#' Temp Server Functions
#'
#' @noRd

productPayoff_server<- function(id, r){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    
  })
}