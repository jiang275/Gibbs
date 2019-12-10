##' Sample sigma^2 given beta0 and beta1
##'
##' Sample one sigma^2 value from its full conditional distribution given beta0 and beta1
##' @title Sample sigma^2 given beta0 and beta1
##' @param draws matrix containing at least one set of beta0 and beta1 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @return an updated sigma^2 value
##' @author Meilin Jiang
##' @references
##' Lesaffre, E. &Lawson, A. (2012). Bayesian Biostatistics. John Wiley & Sons, Ltd. Retrieved from https://onlinelibrary.wiley.com/doi/book/10.1002/9781119942412
##'
##' Wakefield, J. (2013). Bayesian and Frequentist Regression Methods. Springer Series in Statistics. Retrieved from https://link.springer.com/book/10.1007/978-1-4419-0925-1
##' @import stats
##' @export
##' @examples
##' data(simulated_data)
##' draws <- matrix(NA, 100, 3, dimnames = list(NULL, c("sigma2", "beta0", "beta1")))
##' draws[1, ] <- 1
##' sigma2_samp(draws=draws,x=simulated_data$x,y=simulated_data$y,n=1000)
sigma2_samp <- function(draws, x, y, n) {
    b0 <- draws[, "beta0"][max(which(!is.na(draws[, "beta0"])))]
    b1 <- draws[, "beta1"][max(which(!is.na(draws[, "beta1"])))]
    sb2 <- 1/n * sum((y - b0 - b1 * x)^2)
    u <- rchisq(1, df = n)
    n * sb2/u

}
