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
    bslib::layout_columns(
      style = "padding-top: 5px;",
      col_widths = c(3, 9),
      
      ## Information
      bslib::card(bslib::card_header("Information"),
                  bslib::layout_column_wrap(
                    bslib::card(
                      shiny::p("StructuredEdge is an innovative platform that transforms the creation and management of structured financial products. By integrating options, forwards, and swaps with advanced pricing methodologies, it streamlines complexity and provides critical insights into payoffs, costs, and market dynamics. The platform's robust simulation tools, including diffusion, mean-reversion, and Levy mean-reversion models, empower users to test their structured products under various market conditions and refine their strategies accordingly. These capabilities ensure that financial professionals can make informed decisions while adapting to changing economic environments. Beyond its technical strengths, StructuredEdge is designed with user accessibility in mind. Its intuitive interface facilitates the seamless grouping of derivatives, rapid simulation testing, and detailed analysis, catering to both seasoned experts and newcomers in the financial domain. By consolidating diverse financial instruments into a single cohesive platform, StructuredEdge offers a powerful, user-friendly solution for optimizing structured product strategies and achieving actionable insights.")),
                    bslib::card(
                      shiny::h4("Brayden Boyko"),
                      shiny::div(icon("envelope"), 
                          "bnboyko@ualberta.ca",
                          shiny::actionButton("copy_email2", icon("clipboard"), class = "btn btn-sm btn-outline-primary")),
                      shiny::div(icon("phone"), 
                          "+1 587 873 1874",
                          shiny::actionButton("copy_phone2", icon("clipboard"), class = "btn btn-sm btn-outline-primary")),
                      shiny::div(icon("linkedin"), 
                          shiny::a("LinkedIn Profile", href = "https://www.linkedin.com/in/brayden-boyko/", target = "_blank"))
                    ), col_widths = c(6,6))
                ),
      ## Edu
      bslib::card(
        bslib::card_header("Contributing Paper"),
        style = "background-color: white;",
        tags$iframe(
          src = "www/Report.html",
          width = "100%",
          height = "700px",
          frameborder = "0"
        )
      )
    )
  )  
}
    
#' dev Server Functions
#'
#' @noRd 
mod_dev_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$copy_email2, {
      writeClipboard("bnboyko@ualberta.ca")
    })
    
    observeEvent(input$copy_phone2, {
      writeClipboard("1+ 587 873 1874")
    })
 
  })
}
    
## To be copied in the UI
# mod_dev_ui("dev_1")
    
## To be copied in the server
# mod_dev_server("dev_1")
