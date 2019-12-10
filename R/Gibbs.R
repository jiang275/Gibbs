##' Gibbs Sampling for Simple Linear Regression
##'
##' Use Gibbs sampler to sample parameters of linear regression (sigma2, beta0, and beta1) from full conditionals to determine their posterior distributions.
##' Sample B iterations of sigma2, beta0, and beta1 values from their full conditional distributions.
##' @title Gibbs Sampling for Simple Linear Regression
##' @param x covariate
##' @param y response variable
##' @param method a choice of the following Gibbs sampling methods: deterministic, random, reversible, or block
##' @param sigma2_initial initial value for sigma^2. MLE is used if not supplied
##' @param beta0_initial initial value for beta0. MLE is used if not supplied
##' @param beta1_initial initial value for beta1. MLE is used if not supplied
##' @param B number iterations (sampled values) desired
##' @param seed seed set for random number generation
##' @return a matrix of B vectors of sampled sigma2, beta0, and beta1 values
##' @author Meilin Jiang
##' @references
##' Lesaffre, E. &Lawson, A. (2012). Bayesian Biostatistics. John Wiley & Sons, Ltd. Retrieved from https://onlinelibrary.wiley.com/doi/book/10.1002/9781119942412
##'
##' Wakefield, J. (2013). Bayesian and Frequentist Regression Methods. Springer Series in Statistics. Retrieved from https://link.springer.com/book/10.1007/978-1-4419-0925-1
##' @export
##' @examples
##' data(simulated_data)
##' draws <- Gibbs(x=simulated_data$x,y=simulated_data$y,
##' sigma2_initial=1, beta0_initial=1,beta1_initial=1,method="block",B=1000)
##' apply(draws, 2, mean)
Gibbs <- function(x, y, method = "deterministic", sigma2_initial = NA, beta0_initial = NA,
    beta1_initial = NA, B = 1000, seed = 123) {
    set.seed(seed)
    n <- length(x)
    draws <- matrix(NA, B, 3, dimnames = list(NULL, c("sigma2", "beta0", "beta1")))
    fit <- lm(y ~ x)
    # use lSE as starting values if not given
    if (sum(is.na(c(sigma2_initial, beta0_initial, beta1_initial))) > 0) {

        draws[1, ] <- c((summary(fit)$sigma)^2, coef(fit)[1], coef(fit)[2])
    } else {
        draws[1, ] <- c(sigma2_initial, beta0_initial, beta1_initial)
    }

    if (method == "deterministic") {
        draws <- deterministic(draws = draws, x = x, y = y, n = n, B = B)
    } else if (method == "random") {
        draws <- random(draws = draws, x = x, y = y, n = n)
    } else if (method == "reversible") {
        draws <- reversible(draws = draws, x = x, y = y, n = n, B = B)
    } else if (method == "block") {
        draws <- block(draws = draws, x = x, y = y, n = n, B = B, fit = fit)
    } else {
        print("Please choose one of following Gibbs sampling method:deterministic,deterministic,reversible, or block")
    }

    return(draws)

}
