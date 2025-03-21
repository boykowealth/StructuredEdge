#' creationZone UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
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
                      
                      ## UNDERLYING ASSET
                      shiny::selectInput(
                        inputId = ns("asset_select"),
                        label = "Underlying Asset:",
                        choices = c("Equity", "Fixed Income", "Foreign Exchange", "Commodity"),
                        selected = c("Equity"),
                        multiple = FALSE,
                        selectize = TRUE,
                        width = "80%"
                      ),
                      
                      ## DERIVATIVE TYPE
                      shiny::selectInput(
                        inputId = ns("derivType_select"),
                        label = "Derivative Type:",
                        choices = c("US1M", "US3M", "US6M", "US1Y", "US2Y", "US5Y", "US7Y", "US10Y", "US20Y", "US30Y"),
                        selected = c("US2Y"),
                        multiple = FALSE,
                        selectize = TRUE,
                        width = "80%"
                      ),
                      
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
                        inputId = "update_asset_button",
                        label = "Update Position",
                        width = "80%"
                      )
                      
        ),
        shiny::column(6,
                      
                      ## DERIVATIVE
                      shiny::selectInput(
                        inputId = ns("deriv_select"),
                        label = "Derivative:",
                        choices = c("Option", "Forward", "Swap", "Exotic", "Asset"),
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
                        inputId = "delete_asset_button",
                        label = "Delete Position",
                        width = "80%"
                      )
                      
                      
        )
      )
      
      
    ),
    ## POSITION EDIT - LEFT <END>
    
    ## PORTFOLIO MANAGEMNT - RIGHT <START>
    bslib::card(
      bslib::card_header("Product Positions")
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
    
    ## DATAFRAMES <START>
    
    df.params <- dplyr::tibble(
      Param = c("Spot Price", "Strike Price", "Time to Maturity", 
                "Risk-Free Interest Rate", "Volatility", "Cost of Carry", 
                "Exchange Rate", "Fixed Rate", "Floating Rate",
                "Frequency"
      ),
      Option = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0),
      Forward = c(1, 0, 1, 1, 0, 1, 0, 0, 0, 0),
      Swap = c(1, 0, 1, 0, 0, 1, 0, 1, 1, 0),
      Exotic = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      Asset = c(1, 0, 1, 0, 0, 1, 0, 0, 0, 0)
    )
    
    df.models <- dplyr::tibble(
      Model = c(
        "Black-Scholes", "Binomial Tree", "Financial", "Physical", 
        "Interest Rate", "Exchange Rate", "Credit Default", "Heston", 
        "SABR", "Hull-White", "Dupires", "Auto"
      ),
      Option = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Forward = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      Swap = c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0),
      Exotic = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      Asset = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    )
    
    ## DATAFRAMES <END>
    
    ## LISTS <START>
    
    params <- shiny::reactiveValues(
      asset = NULL,
      deriv = NULL,
      derivType = NULL,
      model = NULL,
      position = NULL,
      nominal = NULL
    )
    
    list.params <- shiny::reactive({
      
      df.params %>% 
        dplyr::select(Param, dplyr::all_of(params$deriv)) %>% 
        dplyr::filter(!!rlang::sym(params$deriv) == 1) %>% 
        dplyr::pull(Param)  # Extract the "Param" column as a vector
    })
    
    list.models <- shiny::reactive({
      
      df.models %>% 
        dplyr::select(Model, dplyr::all_of(params$deriv)) %>% 
        dplyr::filter(!!rlang::sym(params$deriv) == 1) %>% 
        dplyr::pull(Model)   # Extract the "Model" column as a vector
    })
    
    ## LISTS <END>
    
    ## OBSERVERS <START>
    
    shiny::observeEvent(input$asset_select, {
      params$asset <- input$asset_select
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
    
    ## OBSERVERS <END>
    
    
    ## OUTPUTS <START>
    output$derivParams1 <- shiny::renderUI({
      params <- list.params()
      half <- ceiling(length(params) / 2) ## Used in spliting list
      params_left <- params[1:half] ## Splits front half of list
      
      
      lapply(params_left, function(param) {
        shiny::numericInput(
          inputId = ns(paste0("input_", param)),
          label = paste(param, ":"),
          value = 0,
          min = 0,
          step = 0.01,
          width = "80%"
        )
      })
    })
    
    output$derivParams2 <- shiny::renderUI({
      params <- list.params()
      half <- ceiling(length(params) / 2) ## Used in spliting list
      params_right <- params[(half + 1):length(params)] ## Splits back half of list
      
      lapply(params_right, function(param) {
        shiny::numericInput(
          inputId = ns(paste0("input_", param)),
          label = paste(param, ":"),
          value = 0,
          min = 0,
          step = 0.01,
          width = "80%"
        )
      })
    })
    
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
    
    ## OUTPUTS <END>
    
  })
}
    
## To be copied in the UI
# mod_creationZone_ui("creationZone_1")
    
## To be copied in the server
# mod_creationZone_server("creationZone_1")
