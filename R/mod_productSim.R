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
        plotly::plotlyOutput(ns("simChart")),
        plotly::plotlyOutput(ns("simPayoffChart"))
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
      Param = c("Step Size", "Spot", "Mu", "Sigma", "Time", 
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
    
    df.units <- dplyr::tibble(
      Param = c("Step Size", "Spot", "Mu", "Sigma", "Time", 
                "Theta", "Jump Probability", "Jump Mean", "Jump Sigma"),
      Unit = c("(Years)", "($)", "($)", "($)", "(Years)", 
               "(Days)", "(Decimal)","($)", "($)")
    )
    
    ## PARAMETERS <END>
    
    ## OBSERVERS <START>
    shiny::observeEvent(input$simModel_select, {
      params$model <- input$simModel_select
    })
    
    shiny::observeEvent(input$StepSize_value, {
      params$steps <- input$StepSize_value
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
      
      units <- df.units %>% 
        dplyr::filter(Param %in% params)
      unit_map <- setNames(units$Unit, units$Param)
      
      mapply(function(params_clean, original_param) {
        
        unit_label <- unit_map[[original_param]]
        
        shiny::numericInput(
          inputId = ns(paste0(params_clean, "_value")),
          label = paste(original_param, " ", unit_label, ":"), 
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
        r$simData <- simulate_gbm_single(params$time*252, params$spot, params$mu, params$sigma, params$steps)
      }
      
      if (type == "Mean Reversion"){
        r$simData <- simulate_ou_single(params$time*252, params$spot, params$mu, params$theta, params$sigma, params$steps)
      }
      
      if (type == "Levy Mean Reversion"){
        r$simData <- simulate_ouj_single(params$time*252, params$spot, params$mu, params$theta, params$sigma, params$jProb, params$jMean, params$jSigma, params$steps)
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
      
      ## ESTIMATE PAYOFF SPLINE INTERPOLATION
      
      dfProduct <- as.data.frame(r$productTable)
      
      if (!is.null(dfProduct$Spot)){
        
        # splineFit <- stats::smooth.spline(r$productTable$Spot, r$productTable$Product, df=5) ## ERRORS IN FITTING
        # polyFit <- stats::lm(Product ~ stats::poly(Spot, 4), data = dfProduct) ## ERRORS IN FITTING
        # simSpline <- stats::predict(polyFit, r$simPayoff$Sim)$y
        # simSpline <- predict(polyFit, newdata = data.frame(Spot = r$simPayoff$Sim))
        # simSpline <- stats::predict(polyFit, newdata = data.frame(Spot = r$simPayoff$Sim))
        
        # ## COMPRESS RETURNS BY UNDERLYING USING SIGNMOID FUNCTION
        # sim_min <- min(r$simPayoff$Sim)
        # sim_max <- max(r$simPayoff$Sim)
        # range <- (sim_max - sim_min) * 1.2 ## LEVERED POSITIONS
        # 
        # sigmoid_scale <- function(x) {
        #   sigmoid_value <- 1 / (1 + exp(-0.01 * x))
        #   
        #   vals <- 0 + (sigmoid_value * range) ## STARTS AT ZERO 
        #   
        #   return(vals)
        # }
        
        # k <- ncol(r$payoffTable) - 1 ## MY THOUGHT IS THAT YOU COULD ESTIMATE THE COMPLEXITY OF THE FUNCTION BASED ON NUMBER OF DERIVS IN PRODUCT 
        # productLocal <- abs(r$productTable$Product[which.min(abs(r$productTable$Spot))])
        # print(productLocal)
        # 
        # 
        # modelFit <- r$productTable %>% 
        #   dplyr::mutate(prodRet = (Product / productLocal),
        #                 prodRet = pr
        #                 ) 
        # 
        # kmeans_result <- stats::kmeans(modelFit$prodRet, centers = k)
        # knots <- sort(kmeans_result$centers)
        # 
        # print(knots)
        # 
        # splineFit <- stats::lm(prodRet ~ splines::bs(Spot, knots = knots), data = modelFit) ## FIT A PIECEWISE POLYNOMIAL SPLINE BASED ON COMPLEXITY
        # simSpline <- stats::predict(splineFit, newdata = data.frame(Spot = r$simPayoff$Sim))
        
        # r$simPayoff <- r$simPayoff %>%
        #   dplyr::mutate(
        #     Product = simSpline,
        #     sim = replace(c(NA, diff(Sim) / head(Sim, -1) - 1), is.na(c(NA, diff(Sim) / head(Sim, -1) - 1)), 0),
        #     product = replace(c(NA, diff(Product) / head(Product, -1) - 1), is.na(c(NA, diff(Product) / head(Product, -1) - 1)), 0)
        #   ) %>% 
        #   dplyr::select(Time, Sim, Product, sim, product) %>% 
        #   tidyr::pivot_longer(., cols = c(Sim, Product, sim, product), names_to = "Type", values_to = "Value")
      
        
        ## TEMPORARY FIX -> EXTEND WITH PEICEWISE FUNCTIONS TO ACCOUNT FOR VALUES > 100% (TO BE ADDED SPRING 2025)
        r$simPayoff <- r$simData %>% 
          dplyr::mutate(Spot = round((Price / dplyr::first(Price)) - 1, 4)) %>% 
          tidyr::fill(Spot, .direction = "up")
        
        stdProdTable <- r$productTable %>% 
          dplyr::mutate(Spot = round(Spot, 4))
        
        epsilon <- 1e-6
        
        r$modelFit <- r$simPayoff %>% 
          dplyr::left_join(stdProdTable, by = "Spot") %>% 
          tidyr::fill(Product, .direction = "down") %>% 
          dplyr::mutate(
            product = ifelse(is.na(dplyr::lag(Product)), 0, (Product - dplyr::lag(Product))),
            Sim = dplyr::if_else(dplyr::row_number() == 1, Product, dplyr::first(Product) * (1 + Spot)), ## SCALE SIMULATED CHANGES 
            sim = ifelse(is.na(dplyr::lag(Sim)), 0, (Sim - dplyr::lag(Sim)))
          
            # product = ifelse(is.na(dplyr::lag(Product)), 0, (Product / dplyr::lag(Product)) - 1),
            # Product = (Product / dplyr::first(Product)) - 1,
            # Sim = Spot,
            # sim = c(0, diff(Spot) / head(Spot, -1))
          ) %>% 
          tidyr::pivot_longer(., cols = c(Sim, Product, sim, product), names_to = "Type", values_to = "Value")
        
        output$simPayoffChart <- plotly::renderPlotly({
          df <- r$modelFit
          
          df1 <- df %>% 
            dplyr::filter(Type == c("Sim", "Product"))
          
          df2 <- df %>% 
            dplyr::filter(Type == c("sim", "product"))

          p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = Time, y = Value, color = Type)) +
            ggplot2::geom_line(size = 0.5) +
            ggplot2::labs(
              title = "",
              x = "Time Step",
              y = "Percent Change"
            ) +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
              plot.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
              panel.grid.major = ggplot2::element_line(color = "#32434f", size = 0.1, linetype = 2),
              axis.text = ggplot2::element_text(color = "#193244"),
              axis.title = ggplot2::element_text(color = "#193244", size = 10),
              panel.border = ggplot2::element_rect(color = "#193244", fill = NA, size = 0.3),
              axis.line = ggplot2::element_line(color = "#193244"),
              legend.position = "bottom",
              legend.text = ggplot2::element_text(size = 9, color = "#193244"),
              legend.title = ggplot2::element_text(size = 11, face = "bold", color = "#193244"),
              legend.background = ggplot2::element_rect(fill = "#ededeb", color = "#193244")
            ) +
            ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
            ggplot2::scale_color_manual(values = c("Product" = "#aeb8bf", "Sim" = "#193244"))
          
          p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = Value, fill = Type)) +
            ggplot2::geom_histogram(
              bins = 30, 
              alpha = 0.7, 
              color = "#193244", 
              position = "identity"
            ) +
            ggplot2::labs(
              title = "",
              x = "Percent Change",
              y = "Frequency"
            ) +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
              plot.background = ggplot2::element_rect(fill = "#ededeb", color = NA),
              panel.grid.major = ggplot2::element_line(color = "#32434f", size = 0.1, linetype = 2),
              axis.text = ggplot2::element_text(color = "#193244"),
              axis.title = ggplot2::element_text(color = "#193244", size = 10),
              panel.border = ggplot2::element_rect(color = "#193244", fill = NA, size = 0.3),
              axis.line = ggplot2::element_line(color = "#193244"),
              legend.position = "bottom",
              legend.text = ggplot2::element_text(size = 9, color = "#193244"),
              legend.title = ggplot2::element_text(size = 11, face = "bold", color = "#193244"),
              legend.background = ggplot2::element_rect(fill = "#ededeb", color = "#193244")
            ) +
            ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
            ggplot2::scale_fill_manual(values = c("product" = "#aeb8bf", "sim" = "#193244"))
          
          p1 <- plotly::ggplotly(p1)
          p2 <- plotly::ggplotly(p2)
          
          plotly::subplot(p1, p2, nrows = 1)
          
        }) ## CREATE PAYOFF CHART
        
      } else{
        print("Missing Product Data")
      }
      
      
    })
    
    ## EXECUTE <END>
  
  })
}
    
## To be copied in the UI
# mod_productSim_ui("productSim_1")
    
## To be copied in the server
# mod_productSim_server("productSim_1")
