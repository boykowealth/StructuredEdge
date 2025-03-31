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
    bslib::layout_columns(
      style = "padding-top: 5px;",
      col_widths = c(3, 9),
      
      ## STATS - PANEL <START>
      bslib::card(
        bslib::card_header("Model Inputs"),
        shiny::selectInput(
          inputId = ns("simModel_select"),
          label = "Simulation Process:",
          choices = c("Diffusion", "Mean Reversion", "Levy Mean Reversion"),
          selected = c("Difusion"),
          multiple = FALSE,
          selectize = TRUE,
          width = "100%"
        ),

        shiny::uiOutput(ns("modelParams")),
        
        shiny::actionButton(
          inputId = ns("sim_button"),
          label = "Simulate Model",
          width = "100%"
        )
      ),
      
      ## POSITIONS - MAIN <START>
      bslib::card(
        bslib::card_header("Simulation Environment"),
        plotly::plotlyOutput(ns("simChart"))
      ),
    )
  )
}
    
#' productSim Server Functions
#'
#' @noRd 
mod_productSim_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    ## PARAMTERS <START>
    
    params <- shiny::reactiveValues(
     model = "Geometric Brownian Motion",
     steps = 0,
     spot = 0,
     mu = 0,
     sigma = 0,
     time = 0,
     theta = 0,
     jProb = 0,
     jMean = 0,
     jSigma = 0
    )
    
    df.params <- dplyr::tibble(
      Param = c("Number of Steps", "Spot", "Mu", "Sigma", "Time", 
                "Theta", "Jump Probability", "Jump Mean", "Jump Sigma"
                ),
      `Diffusion` = c(1, 1, 1, 1, 1, 0, 0, 0, 0),
      `Mean Reversion` = c(1, 1, 1, 1, 1, 1, 0, 0, 0),
      `Levy Mean Reversion` = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    
    list.params <- shiny::reactive({
      
      df.params %>% 
        dplyr::select(Param, dplyr::all_of(params$model)) %>% 
        dplyr::filter(!!rlang::sym(params$model) == 1) %>% 
        dplyr::pull(Param)  # Extract the "Param" column as a vector
    })
    
    ## PARAMETERS <END>
    
    ## OBSERVERS <START>
    shiny::observeEvent(input$simModel_select, {
      params$model <- input$simModel_select
    })
    
    shiny::observeEvent(input$NumberofSteps_value, {
      params$steps <- input$NumberofSteps_value
    })
    
    shiny::observeEvent(input$Spot_value, {
      params$spot <- input$Spot_value
    })
    
    shiny::observeEvent(input$Mu_value, {
      params$mu <- input$Mu_value
    })
    
    shiny::observeEvent(input$Sigma_value, {
      params$sigma <- input$Sigma_value
    })
    
    shiny::observeEvent(input$Time_value, {
      params$time <- input$Time_value
      print(params$time)
    })
    
    shiny::observeEvent(input$Theta_value, {
      params$theta <- input$Theta_value
    })
    
    shiny::observeEvent(input$JumpProbability_value, {
      params$jProb <- input$JumpProbability_value
    })
    
    shiny::observeEvent(input$JumpMean_value, {
      params$jMean <- input$JumpMean_value
    })
    
    shiny::observeEvent(input$JumpSigma_value, {
      params$JSigma <- input$JumpSigma_value
    })
    
    ## OBSERVERS <END>
    
    ## DYNAMIC PARAMETER BOXES <START>
    
    output$modelParams <- shiny::renderUI({
      params <- list.params()
      params_clean <- lapply(params, function(x) gsub(" ", "", x)) ## Creates standard naming convention
      
      mapply(function(params_clean, original_param) {
        shiny::numericInput(
          inputId = ns(paste0(params_clean, "_value")), # Use cleaned names for inputId
          label = paste(original_param, ":"),         # Use original names for label
          value = 0,
          min = 0,
          step = 0.01,
          width = "100%"
        )
      }, params_clean, params, SIMPLIFY = FALSE)
    })
    
    ## DYNAMIC PARAMETER BOXES <END>
    
    ## EXECUTE <START>
    
    shiny::observeEvent(input$sim_button, {

      type <- params$model
      
      if (type == "Diffusion"){
        r$simData <- simulate_gbm_single(params$steps, params$spot, params$mu, params$sigma, params$time)
      }
      
      if (type == "Mean Reversion"){
        r$simData <- simulate_ou_single(params$steps, params$spot, params$mu, params$theta, params$sigma, params$time)
      }
      
      if (type == "Levy Mean Reversion"){
        r$simData <- simulate_ouj_single(params$steps, params$spot, params$mu, params$theta, params$sigma, params$jProb, params$jMean, params$jSigma, params$time)
      }
      
      #print(r$simData)
      
      output$simChart <- plotly::renderPlotly({
        df <- r$simData
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Price)) +
          ggplot2::geom_line(color = "#193244", size = 1.05) +
          ggplot2::labs(
            title = "",
            x = "Time Step",
            y = "Spot Value"
          ) +
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
            plot.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
            panel.grid.major = ggplot2::element_line(color = "#32434f", size = 0.1, linetype = 2),
            axis.text = ggplot2::element_text(color = "#193244"),
            axis.title = ggplot2::element_text(color = "#193244", size = 10),
            panel.border = ggplot2::element_rect(color = "#193244", fill = NA, size = 0.3),
            axis.line = ggplot2::element_line(color = "#193244")
          )
        
        plotly::ggplotly(p)
      }) ## CREATE PAYOFF CHART
      
    })
    
    ## EXECUTE <END>
  
  })
}
    
## To be copied in the UI
# mod_productSim_ui("productSim_1")
    
## To be copied in the server
# mod_productSim_server("productSim_1")
