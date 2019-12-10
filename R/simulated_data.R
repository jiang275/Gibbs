#' Simulated data set
#'
#' A sumulated dataset containing the repsonse y and covariate x.
#'
#' True values: beta0 = 2, beta1 = 0.7, sigma^2 = 1
#'
#' Generation process:
#'
#' set.seed(123)
#'
#' x <- rnorm(1000,8,4)
#'
#' y <- 2 + 0.7*x + rnorm(1000,0,1)
#'
#' simulated_data <- data.frame(x,y)
#'
#' @format A data frame with 1000 rows and 2 variables:
#' \describe{
#'   \item{x}{covariate}
#'   \item{y}{response variable}
#'   ...
#' }

"simulated_data"
