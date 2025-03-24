#' creationZone UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @useDynLib StructuredEdge, .registration = TRUE
#'
#' @importFrom shiny NS tagList 
mod_creationZone_ui <-  function(id){
  ns <- NS(id)
  
  bslib::layout_columns(
    style = "padding-top: 5px;",
    col_widths = c(6, 6),
    
    ## POSITION EDIT - LEFT <START>
    bslib::card(
      bslib::card_header("Edit Position"),
      
      shiny::fluidRow(
        shiny::column(6, 
                      
                      ## UNDERLYING POSITION NAME
                      shiny::textAreaInput(
                        inputId = ns("posName_text"),
                        label = "Position Name:",
                        value = "",
                        width = "80%",
                        height = "17px"
                      ),
                      
                      ## DERIVATIVE TYPE
                      shiny::uiOutput(ns("types")),
                      
                      ## POSITION
                      shiny::selectInput(
                        inputId = ns("pos_select"),
                        label = "Position:",
                        choices = c("Long", "Short"),
                        selected = c("Long"),
                        multiple = FALSE,
                        selectize = TRUE,
                        width = "80%"
                      ),
                      
                      ## PARAMS 1 (Left Split)
                      shiny::uiOutput(ns("derivParams1")),
                      
                      shiny::actionButton(
                        inputId = ns("add_asset_button"),
                        label = "Update Position",
                        width = "80%"
                      )
                      
        ),
        shiny::column(6,
                      
                      ## DERIVATIVE
                      shiny::selectInput(
                        inputId = ns("deriv_select"),
                        label = "Derivative:",
                        choices = c("Option", "Forward", "Swap", "Asset"),
                        selected = c("Option"),
                        multiple = FALSE,
                        selectize = TRUE,
                        width = "80%"
                      ),
                      
                      ## PRICING MODEL
                      shiny::uiOutput(ns("models")),
                      
                      ## NOMINAL VALUE
                      shiny::numericInput(
                        inputId = ns('nominal_num'), 
                        label = 'Nominal Value:',
                        value = 100000,
                        min = 1,
                        step = 1,
                        width = "80%"
                      ),
                      
                      ## PARAMS 2 (Right Split)
                      shiny::uiOutput(ns("derivParams2")),
                      
                      ## DELETE BUTTON
                      shiny::actionButton(
                        inputId = "reset_asset_button",
                        label = "Reset Position",
                        width = "80%"
                      )
        )
      )
      
      
    ),
    ## POSITION EDIT - LEFT <END>
    
    ## PORTFOLIO MANAGEMNT - RIGHT <START>
    bslib::card(
      bslib::card_header("Product Positions"),
      shiny::p("Test"),
      DT::DTOutput(ns("masterView"))
    )
    ## PORTFOLIO MANAGEMNT - RIGHT <END>
    
  )
}
    
#' creationZone Server Functions
#'
#' @noRd 
mod_creationZone_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    ## DATAFRAMES LOCAL <START>
        # (NO small r pass - only used in filters on this page)
    
    df.params <- dplyr::tibble(
      Param = c("Spot Price", "Strike Price", "Time to Maturity", "Risk Free Interest Rate", "Volatility",
                "Cost of Carry", "Up Factor", "Down Factor", "Probability", "Steps",
                "Foreign Rate", "Domestic Rate", "Rate T1", "Rate T2", "Time 1", "Time 2",
                "Fixed Spot", "Floating Spot", "Fixed Rate", "Floating Rate"),
      `Black-Scholes` = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Binomial Tree` = c(1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Financial Forward` = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Commodity Forward` = c(1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Forward Rate Agreement` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
      `Exchange Rate Forward` = c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      `Commodity Swap` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0),
      `Interest Rate Swap` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
      `Exchange Rate Swap` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
      `Equity Swap` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0),
      `Credit Default Swap` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
      `Variance Swap` = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
      `Auto` = c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    
    df.models <- dplyr::tibble(
      Model = c(
        "Black-Scholes", "Binomial Tree", "Financial Forward", "Commodity Forward",
        "Forward Rate Agreement", "Exchange Rate Forward", "Commodity Swap", "Interest Rate Swap",
        "Exchange Rate Swap", "Equity Swap", "Credit Default Swap", "Variance Swap", "Auto"
      ),
      Option = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Forward = c(0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
      Swap = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0),
      Asset = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    )
    
    df.type <- dplyr::tibble(
      Type = c("Call", "Put", "Fixed", "Float", "Auto"),
      Option = c(1, 1, 0, 0, 0),
      Forward = c(0, 0, 0, 0, 1),
      Swap = c(0, 0, 1, 1, 0),
      Asset = c(0, 0, 0, 0, 1)
    )
    
    ## DATAFRAMES LOCAL <END>
    
    ## DATAFRAMES GLOBAL <START>
    
    
    
    ## DATAFRAMES GLOBAL <END>
    
    ## LISTS <START>
    randomNames <- c("apple", "banana", "cherry", "date", "elderberry", "fig", "grape", "honeydew", "kiwi", "lemon",
                     "mango", "nectarine", "orange", "papaya", "quince", "raspberry", "strawberry", "tangerine", "ugli", "vanilla",
                     "watermelon", "xigua", "yam", "zucchini", "apricot", "blueberry", "cranberry", "dragonfruit", "eggplant", "feijoa", ## USED IN RANDOM NAME GEN
                     "guava", "huckleberry", "imbe", "jackfruit", "kumquat", "lime", "mulberry", "navel", "olive", "peach",
                     "persimmon", "plum", "pomegranate", "redcurrant", "salak", "tomato", "uvaia", "voavanga", "wolfberry", "yumberry")
    
    params <- shiny::reactiveValues(
      pos = 0,
      deriv = 0,
      derivType = 0,
      model = 0,
      position = 0,
      nominal = 0,
      spot = 0,
      strike = 0,
      t2m = 0,
      rf = 0,
      sigma = 0,
      costCarry = 0,
      upFactor = 0,
      downFactor = 0,
      prob = 0,
      steps = 0,
      rForeign = 0,
      rDomestic = 0,
      r1 = 0,
      r2 = 0,
      t1 = 0,
      t2 = 0,
      fixSpot = 0,
      floatSpot = 0,
      fixRate = 0,
      floatRate = 0
    )
    
    list.params <- shiny::reactive({
      
      df.params %>% 
        dplyr::select(Param, dplyr::all_of(params$model)) %>% 
        dplyr::filter(!!rlang::sym(params$model) == 1) %>% 
        dplyr::pull(Param)  # Extract the "Param" column as a vector
    })
    
    list.models <- shiny::reactive({
      
      df.models %>% 
        dplyr::select(Model, dplyr::all_of(params$deriv)) %>% 
        dplyr::filter(!!rlang::sym(params$deriv) == 1) %>% 
        dplyr::pull(Model)   # Extract the "Model" column as a vector
    })
    
    list.types <- shiny::reactive({
      
      df.type %>% 
        dplyr::select(Type, dplyr::all_of(params$deriv)) %>% 
        dplyr::filter(!!rlang::sym(params$deriv) == 1) %>% 
        dplyr::pull(Type)   # Extract the "Model" column as a vector
    })
    
    
    ## LISTS <END>
    
    ## r PASSING <START>
    
    shiny::observeEvent(input$add_asset_button, {
      
      tmpTable <- ## Temporary Table For User Selections (Is rbind to master table)
        dplyr::tibble(
          pos = params$pos,
          deriv = params$deriv,
          derivType = params$derivType,
          model = params$model,
          position = params$position,
          nominal = params$nominal,
          spot = params$spot,
          strike = params$strike,
          t2m = params$t2m,
          rf = params$rf,
          sigma = params$sigma,
          costCarry = params$costCarry,
          upFactor = params$upFactor,
          downFactor = params$downFactor,
          prob = params$prob,
          steps = params$steps,
          rForeign = params$rForeign,
          rDomestic = params$rDomestic,
          r1 = params$r1,
          r2 = params$r2,
          t1 = params$t1,
          t2 = params$t2,
          fixSpot = params$fixSpot,
          floatSpot = params$floatSpot,
          fixRate = params$fixRate,
          floatRate = params$floatRate,
          )
      
      if (is.null(r$masterTable)) {
        r$masterTable <- tmpTable  ## Initialize masterTable
      } else {
        r$masterTable <- rbind(r$masterTable, tmpTable)  ## append table if exists
      }
      
      r$viewTable <- r$masterTable %>% 
        dplyr::select(pos, deriv, derivType, model, position, nominal) %>% ## Generate standard viewing table for UI
        dplyr::rename(
          Name = pos,
          Derviative = deriv,
          Type = derivType,
          Model = model,
          Position = position,
          Nominal = nominal
        )
      
      
      output$masterView <- DT::renderDT({
        DT::datatable(r$viewTable,
                      options = list(paging = FALSE,
                                     dom = "ft"
                      )
                      ) ## update display table
      })
      
      
      ## CREATE PAYOFFS
      selected_model <- params$model
      
          ### list of available pricing models
      model_functions <- list(
        "Black-Scholes" = "blackScholes",
        "Binomial Tree" = "binomialTree",
        "Financial Forward" = "finForwardContract",
        "Commodity Forward" = "physForwardContract",
        "Forward Rate Agreement" = "irForward",
        "Exchange Rate Forward" = "exchangeForward",
        "Commodity Swap" = "physicalSwap"
      )
      
          ### list of parameters per model
      param_map <- list(
        "Black-Scholes" = c("spot", "strike", "t2m", "rf", "sigma", "costCarry"),
        "Binomial Tree" = c("spot", "strike", "t2m", "rf", "upFactor", "downFactor", "prob", "steps"),
        "Financial Forward" = c("spot", "t2m", "rf"),
        "Commodity Forward" = c("spot", "t2m", "rf", "costCarry"),
        "Forward Rate Agreement" = c("r1", "r2", "t1", "t2"),
        "Exchange Rate Forward" = c("spot", "t2m", "rdomestic", "rforeign"),
        "Commodity Swap" = c("nominal", "fixedSpot", "floatSpot")
      )
      
      if (selected_model %in% names(model_functions)) {
        
        function_name <- model_functions[[selected_model]]
        required_params <- param_map[[selected_model]]
        model_params <- lapply(required_params, function(param) params[[param]])
        payoffResult <- do.call(get(function_name), model_params)
      } else {
        print("Invalid model selection")
      }
      
      if (is.null(r$payoffTable)) {
        r$payoffTable <- payoffResult  ## Initialize Payoff Tables
      } else {
        r$payoffTable <- dplyr::left_join(r$payoffTable, payoffResult, by="Spot")
      }
      
      print(r$payoffTable)
      
      
      # ## RESET VALUES (AFTER APPENDING TABLES)
      # params$pos <- 0
      # params$deriv <- 0  
      # params$derivType <- 0  
      # params$model <- 0  
      # params$position <- 0  
      # params$nominal <- 0  
      # params$spot <- 0  
      # params$strike <- 0  
      # params$t2m <- 0  
      # params$sigma <- 0  
      # params$costCarry <- 0  
      # params$upFactor <- 0  
      # params$downFactor <- 0  
      # params$prob <- 0  
      # params$steps <- 0  
      # params$rForeign <- 0  
      # params$rDomestic <- 0  
      # params$r1 <- 0  
      # params$r2 <- 0  
      # params$t1 <- 0  
      # params$t2 <- 0  
      # params$fixSpot <- 0  
      # params$floatSpot <- 0  
      # params$fixRate <- 0  
      # params$floatRate <- 0  
    
      
        
    })
    
    ## r DATA PASSING <END>
  
    ## OBSERVERS <START>
    
    shiny::observeEvent(input$posName_text, {
      if (input$posName_text == ""){
        params$pos <- paste(sample(randomNames,1),"-",sample(1:1000, 1)) ## Automatically Create Name (ASK PHIL WHAT IS CAUSING THIS ERROR)
      } else{
        params$pos <- input$posName_text
      }
    })
    
    shiny::observeEvent(input$deriv_select, {
      params$deriv <- input$deriv_select
    })
    
    shiny::observeEvent(input$derivType_select, {
      params$derivType <- input$derivType_select
    })
    
    shiny::observeEvent(input$pricing_select, {
      params$model <- input$pricing_select
    })
    
    shiny::observeEvent(input$pos_select, {
      params$position <- input$pos_select
    })
    
    shiny::observeEvent(input$nominal_num, {
      params$nominal <- input$nominal_num
    })
    
    shiny::observeEvent(input$SpotPrice_value, {
      params$spot <- input$SpotPrice_value
    })
    
    shiny::observeEvent(input$StrikePrice_value, {
      params$strike <- input$strike_value
    })
    
    shiny::observeEvent(input$TimetoMaturity_value, {
      params$t2m <- input$TimetoMaturity_value
    })
    
    shiny::observeEvent(input$RiskFreeInterestRate_value, {
      params$rf <- input$RiskFreeInterestRate_value
    })
    
    shiny::observeEvent(input$Volatility_value, {
      params$sigma <- input$Volatility_value
    })
    
    shiny::observeEvent(input$CostofCarry_value, {
      params$costCarry <- input$CostofCarry_value
    })
    
    shiny::observeEvent(input$UpFactor_value, {
      params$upFactor <- input$UpFactor_value
    })
    
    shiny::observeEvent(input$DownFactor_value, {
      params$downFactor <- input$DownFactor_value
    })
    
    shiny::observeEvent(input$Probability_value, {
      params$prob <- input$Probability_value
    })
    
    shiny::observeEvent(input$Steps_value, {
      params$steps <- input$Steps_value
    })
    
    shiny::observeEvent(input$ForeignRate_value, {
      params$rForeign <- input$ForeignRate_value
    })
    
    shiny::observeEvent(input$DomesticRate_value, {
      params$rDomestic <- input$DomesticRate_value
    })
    
    shiny::observeEvent(input$RateT1_value, {
      params$r1 <- input$Ratet1_value
    })
    
    shiny::observeEvent(input$RateT2_value, {
      params$r2 <- input$RateT2_value
    })
    
    shiny::observeEvent(input$Time1_value, {
      params$t1 <- input$Time1_value
    })
    
    shiny::observeEvent(input$Time2_value, {
      params$t2 <- input$Time2_value
    })
    
    shiny::observeEvent(input$FixedSpot_value, {
      params$fixSpot <- input$FixedSpot_value
    })
    
    shiny::observeEvent(input$FloatingSpot_value, {
      params$floatSpot <- input$FloatingSpot_value
    })
    
    shiny::observeEvent(input$FixedRate_value, {
      params$fixRate <- input$FixedRate_value
    })
    
    shiny::observeEvent(input$FloatingRate_value, {
      params$floatRate <- input$FloatingRate_value
    })
    
    
    ## OBSERVER <END>
    
    ## OUTPUTS <START>
    
    observe({
      r$placeHolder <- dplyr::tibble(
        Name = "None",
        Derviative = "None",
        Type = "None",
        Model = "None",
        Position = "None",
        Nominal = 0
      )
    })
    
    output$masterView <- DT::renderDT({
      DT::datatable(r$placeHolder,
                    options = list(paging = FALSE,
                                   dom = "ft"
                                   )
                    ) ## init viewable table
    })
    
        ## AUTO GENERATING PARAMETER VALUE BOXES <START>
    
    output$derivParams1 <- shiny::renderUI({
      params <- list.params()
      half <- ceiling(length(params) / 2) ## Used in splitting list
      params_left <- params[1:half] ## Splits front half of list
      params_left_clean <- lapply(params_left, function(x) gsub(" ", "", x)) ## Creates standard naming convention
      
      mapply(function(clean_param, original_param) {
        shiny::numericInput(
          inputId = ns(paste0(clean_param, "_value")), # Use cleaned names for inputId
          label = paste(original_param, ":"),         # Use original names for label
          value = 0,
          min = 0,
          step = 0.01,
          width = "80%"
        )
      }, params_left_clean, params_left, SIMPLIFY = FALSE)
    })
    
    output$derivParams2 <- shiny::renderUI({
      params <- list.params()
      half <- ceiling(length(params) / 2) ## Used in spliting list
      params_right <- params[(half + 1):length(params)] ## Splits back half of list
      params_right_clean <- lapply(params_right, function(x) gsub(" ", "", x)) ## Creates standard naming convention
      
      mapply(function(clean_param, original_param) {
        shiny::numericInput(
          inputId = ns(paste0(clean_param, "_value")), # Use cleaned names for inputId
          label = paste(original_param, ":"),         # Use original names for label
          value = 0,
          min = 0,
          step = 0.01,
          width = "80%"
        )
      }, params_right_clean, params_right, SIMPLIFY = FALSE)
    })
        
        ## AUTO GENERATING PARAMETER VALUE BOXES <END> 
    
    output$models <- shiny::renderUI({
      
      models <- list.models()
      
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
    
    output$types <- shiny::renderUI({
      
      types <- list.types()
      
      shiny::selectInput(
        inputId = ns("type_select"),
        label = "Derivative Type:",
        choices = types,
        selected = types[1],
        multiple = FALSE,
        selectize = FALSE,
        width = "80%"
      )
      
    })
    
    ## OUTPUTS <END>
    
  })
}
    
## To be copied in the UI
# mod_creationZone_ui("creationZone_1")
    
## To be copied in the server
# mod_creationZone_server("creationZone_1")
