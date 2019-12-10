##' Sample beta1 given beta0 and sigma^2
##'
##' Sample one beta1 value from its full conditional distribution given beta0 and sigma^2
##' @title Sample beta1 given beta0 and sigma^2
##' @param draws matrix containing at least one set of beta0 and sigma^2 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @return an updated beta1 value
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
##' b1_samp(draws=draws,x=simulated_data$x,y=simulated_data$y,n=1000)
b1_samp <- function(draws, x, y, n) {
    b0 <- draws[, "beta0"][max(which(!is.na(draws[, "beta0"])))]
    sigma2 <- draws[, "sigma2"][max(which(!is.na(draws[, "sigma2"])))]
    rb0 <- sum((y - b0) * x)/c(t(x) %*% x)
    rnorm(1, mean = rb0, sd = sqrt(sigma2/c(t(x) %*% x)))

}
