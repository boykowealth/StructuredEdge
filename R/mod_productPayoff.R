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
    bslib::layout_columns(
      style = "padding-top: 5px;",
      col_widths = c(4, 4, 4),
      
      ## STATS - LEFT <START>
      bslib::card(
        bslib::card_header("Product Statistics")
      ),
      
      ## POSITIONS - CENTER <START>
      bslib::card(
        bslib::card_header("Product Breakdown")
      ),
      
      ## POSITIONS - RIGHT <START>
      bslib::card(
        bslib::card_header("Product Breakdown")
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(8, 4),
      
      bslib::card(
        bslib::card_header("Product Payoff"),
        plotly::plotlyOutput(ns("prodPayoff"))
      ),
      
      ## POSITIONS - LEFT <START>
      bslib::card(
        bslib::card_header("Product Positions"),
        DT::DTOutput(ns("masterView2"))
      )
    )
)
}
    
#' productPayoff Server Functions
#'
#' @noRd 
mod_productPayoff_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$prodPayoff <- plotly::renderPlotly({
      df <- r$productTable %>% 
        tidyr::drop_na()
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Spot, y = Product)) +
        ggplot2::geom_line(color = "#193244", size = 1.05) +
        ggplot2::labs(
          title = "",
          x = "Underlying Change (Percentage)",
          y = "Product P&L"
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "#193244", size = 0.3) +
        ggplot2::geom_vline(xintercept = 0, color = "#193244", size = 0.3) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
          plot.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
          panel.grid.major = ggplot2::element_line(color = "#32434f", size = 0.1, linetype = 2),
          axis.text = ggplot2::element_text(color = "#193244"),
          axis.title = ggplot2::element_text(color = "#193244", size = 10),
          panel.border = ggplot2::element_rect(color = "#193244", fill = NA, size = 0.3),
          axis.line = ggplot2::element_line(color = "#193244")
        ) +
        ggplot2::scale_x_continuous(labels = scales::percent_format()) +
        ggplot2::scale_y_continuous(labels = scales::dollar_format())
      
      plotly::ggplotly(p)
    }) ## CREATE PAYOFF TABLE
 
    output$masterView2 <- DT::renderDT({
      DT::datatable(r$viewTable,
                    options = list(paging = FALSE,
                                   dom = "ft"
                    )
      ) ## update display table
    })
    
  })
}
    
## To be copied in the UI
# mod_productPayoff_ui("productPayoff_1")
    
## To be copied in the server
# mod_productPayoff_server("productPayoff_1")
