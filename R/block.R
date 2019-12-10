##' Block Gibbs sampler
##'
##' The d=3 dimensions (sigma2, beta0, and beta1) are are split up into 2 blocks of parameters in sampling.
##' Sample B iterations of sigma2, beta0, and beta1 values. Values of sigma^2 are sampled from its full conditional distribution, and then the vector of (beta0, beta1) is sampled simutaneously from the multivariate normal distribution given sigma^2.
##' @title Block Gibbs sampler
##' @param draws matrix containing one set of sigma2, beta0, and beta1 as initial values
##' @param x covariate
##' @param y response variable
##' @param n number of measurements
##' @param B number iterations (sampled values) desired
##' @param fit a linear model of y~x given by lm(y~x) to get maximum likelihood estimators of parameters
##' @return a matrix of B vectors of sampled sigma2, beta0, and beta1 values
##' @author Meilin Jiang
##' @references
##' Lesaffre, E. &Lawson, A. (2012). Bayesian Biostatistics. John Wiley & Sons, Ltd. Retrieved from https://onlinelibrary.wiley.com/doi/book/10.1002/9781119942412
##'
##' Wakefield, J. (2013). Bayesian and Frequentist Regression Methods. Springer Series in Statistics. Retrieved from https://link.springer.com/book/10.1007/978-1-4419-0925-1
##' @import MASS
##' @export
##' @examples
##' data(simulated_data)
##' fit <- lm(simulated_data$y ~ simulated_data$x)
##' draws <- matrix(NA, 100, 3, dimnames = list(NULL, c("sigma2", "beta0", "beta1")))
##' draws[1, ] <- 1
##' block(draws = draws,x=simulated_data$x,y=simulated_data$y,n=1000,B=100,fit=fit)
#library(MASS)
block <- function(draws, x, y, n, B, fit) {
    for (i in 2:B) {
        X <- cbind(1, x)
        draws[i, "sigma2"] <- sigma2_samp(draws = draws, x = x, y = y, n = n)
        draws[i, c("beta0", "beta1")] <- mvrnorm(n = 1, mu = coef(fit), Sigma = (draws[i -
            1, "sigma2"] * solve(t(X) %*% X)))
    }
    return(draws)
}
