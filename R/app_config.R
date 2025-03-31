#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
#' @useDynLib StructuredEdge, .registration = TRUE
#' 
app_sys <- function(...) {
  system.file(..., package = "StructuredEdge")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
#' 

library(magrittr)
library(shiny)
library(golem)
library(dplyr)
library(Rcpp)
library(DT)
library(ggplot2)
library(plotly)

Rcpp::sourceCpp("src/blackScholes.cpp")
Rcpp::sourceCpp("src/binomialTree.cpp")
Rcpp::sourceCpp("src/financialForward.cpp")
Rcpp::sourceCpp("src/physicalForward.cpp")
Rcpp::sourceCpp("src/irForward.cpp")
Rcpp::sourceCpp("src/excForward.cpp")
Rcpp::sourceCpp("src/financialSwap.cpp")
Rcpp::sourceCpp("src/physicalSwap.cpp")
Rcpp::sourceCpp("src/excSwap.cpp")
Rcpp::sourceCpp("src/varSwap.cpp")
Rcpp::sourceCpp("src/cds.cpp")

Rcpp::sourceCpp("src/simGBM.cpp")

get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
