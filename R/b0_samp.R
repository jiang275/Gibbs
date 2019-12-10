##' Sample beta0 given beta1 and sigma^2
##'
##' Sample one beta0 value from its full conditional distribution given beta1 and sigma^2
##' @title Sample beta0 given beta1 and sigma^2
##' @param draws matrix containing at least one set of beta1 and sigma^2 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @return an updated beta0 value
##' @author Meilin Jiang
##' @references
##' Lesaffre, E. &Lawson, A. (2012). Bayesian Biostatistics. John Wiley & Sons, Ltd. Retrieved from https://onlinelibrary.wiley.com/doi/book/10.1002/9781119942412
##' Wakefield, J. (2013). Bayesian and Frequentist Regression Methods. Springer Series in Statistics. Retrieved from https://link.springer.com/book/10.1007/978-1-4419-0925-1
##' @import stats
##' @export
##' @examples
##' data(simulated_data)
##' draws <- matrix(NA, 100, 3, dimnames = list(NULL, c("sigma2", "beta0", "beta1")))
##' draws[1, ] <- 1
##' b0_samp(draws=draws,x=simulated_data$x,y=simulated_data$y,n=1000)
b0_samp <- function(draws, x, y, n) {
    sigma2 <- draws[, "sigma2"][max(which(!is.na(draws[, "sigma2"])))]
    b1 <- draws[, "beta1"][max(which(!is.na(draws[, "beta1"])))]
    rb1 <- 1/n * sum(y - b1 * x)
    rnorm(1, mean = rb1, sd = sqrt(sigma2/n))
}
