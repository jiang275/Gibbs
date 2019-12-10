##' Reversible Gibbs sampler
##'
##' The d=3 dimensions (sigma2, beta0, and beta1) are visited in a random order.
##' Sample B iterations of sigma2, beta0, and beta1 values from their full conditional distributions in a particular order and then the order is reversed.
##' @title Reversible Gibbs sampler
##' @param draws matrix containing one set of sigma2, beta0, and beta1 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @param B number iterations (sampled values) desired
##' @return a matrix of B vectors of sampled sigma2, beta0, and beta1 values
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
##' reversible(draws = draws,x=simulated_data$x,y=simulated_data$y,n=1000,B=100)
reversible <- function(draws, x, y, n, B) {
    for (i in seq(1, B - 1, 2)) {
        draws[i, "sigma2"] <- sigma2_samp(draws = draws, x = x, y = y, n = n)
        draws[i, "beta0"] <- b0_samp(draws = draws, x = x, y = y, n = n)
        draws[i, "beta1"] <- b1_samp(draws = draws, x = x, y = y, n = n)
        draws[i + 1, "beta1"] <- b1_samp(draws = draws, x = x, y = y, n = n)
        draws[i + 1, "beta0"] <- b0_samp(draws = draws, x = x, y = y, n = n)
        draws[i + 1, "sigma2"] <- sigma2_samp(draws = draws, x = x, y = y, n = n)
    }
    return(draws)
}
