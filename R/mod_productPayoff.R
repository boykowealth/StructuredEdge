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
  bslib::card(
    bslib::card_header("Product Payoff"),
    shiny::plotOutput(ns("prodPayoff"))
  )
)
}
    
#' productPayoff Server Functions
#'
#' @noRd 
mod_productPayoff_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$prodPayoff <- shiny::renderPlot({
      df <- r$productTable %>% 
        tidyr::drop_na()
      
      ggplot2::ggplot(df, ggplot2::aes(x = Spot, y = Product)) +
        ggplot2::geom_smooth(color = "#193244") +
        ggplot2::labs(
          title = "",
          x = "Underlying Change (Percentage)",
          y = "Product P&L",
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "#193244", size = 0.3) +
        ggplot2::geom_vline(xintercept = 0, color = "#193244", size = 0.3) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
          plot.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
          panel.grid.major = ggplot2::element_line(color = "#193244", size = 0.3, linetype = "dotted"),
          axis.text = ggplot2::element_text(color = "#193244"),
          axis.title = ggplot2::element_text(color = "#193244"),
          panel.border = ggplot2::element_rect(color = "#193244", fill = NA, size = 0.3)
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_productPayoff_ui("productPayoff_1")
    
## To be copied in the server
# mod_productPayoff_server("productPayoff_1")
