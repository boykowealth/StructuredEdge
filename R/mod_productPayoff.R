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
    
    shiny::uiOutput(ns("test")),
  )
}
    
#' productPayoff Server Functions
#'
#' @noRd 
mod_productPayoff_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #df_test <- shiny::reactive({
     #r$userTable()
    #})
    
    
    
    output$test <- shiny::renderUI({
      
      #models <- df_test()$type
      models <- r$userTable$type
      
      shiny::selectInput(
        inputId = ns("pricing_select"),
        label = "Pricing Model:",
        choices = models,
        selected = models[1],
        multiple = FALSE,
        selectize = FALSE,
        width = "80%"
      )
      
    })
    
    
 
  })
}
    
## To be copied in the UI
# mod_productPayoff_ui("productPayoff_1")
    
## To be copied in the server
# mod_productPayoff_server("productPayoff_1")
