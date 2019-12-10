##' Random-scan Gibbs sampler
##'
##' The d=3 dimensions (sigma2, beta0, and beta1) are visited in a random order.
##' Sample B iterations of sigma2, beta0, and beta1 values in a random order from their full conditional distributions
##' @title Random-scan Gibbs sampler
##' @param draws matrix containing one set of sigma2, beta0, and beta1 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @return a matrix of vectors of sampled sigma2, beta0, and beta1 values (number of iterations specified as the row of input vector "draws")
##' @author Meilin Jiang
##' @references
##' Lesaffre, E. &Lawson, A. (2012). Bayesian Biostatistics. John Wiley & Sons, Ltd. Retrieved from https://onlinelibrary.wiley.com/doi/book/10.1002/9781119942412
##'
##' Wakefield, J. (2013). Bayesian and Frequentist Regression Methods. Springer Series in Statistics. Retrieved from https://link.springer.com/book/10.1007/978-1-4419-0925-1
##' @export
##' @examples
##' data(simulated_data)
##' draws <- matrix(NA, 100, 3, dimnames = list(NULL, c("sigma2", "beta0", "beta1")))
##' draws[1, ] <- 1
##' random(draws = draws,x=simulated_data$x,y=simulated_data$y,n=1000)
random <- function(draws, x, y, n) {
    while (sum(is.na(draws)) > 0) {
        space <- colnames(draws)[apply(is.na(draws), 2, sum) > 0]
        one <- sample(space, size = 1)
        if (one == "sigma2") {

            draws[, "sigma2"][max(which(!is.na(draws[, "sigma2"]))) + 1] <- sigma2_samp(draws = draws,
                x = x, y = y, n = n)
        } else if (one == "beta0") {
            draws[, "beta0"][max(which(!is.na(draws[, "beta0"]))) + 1] <- b0_samp(draws = draws,
                x = x, y = y, n = n)
        } else if (one == "beta1") {
            draws[, "beta1"][max(which(!is.na(draws[, "beta1"]))) + 1] <- b1_samp(draws = draws,
                x = x, y = y, n = n)
        }
    }
    return(draws)
}
