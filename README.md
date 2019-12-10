# Gibbs
Gibbs Sampling for Simple Linear Regression

## how to install the R package
devtools::install_github("https://github.com/jiang275/Gibbs", build_vignettes = TRUE)

## example
library(Gibbs)

data(simulated_data)

draws <- Gibbs(x=simulated_data$x,y=simulated_data$y,
sigma2_initial=1, beta0_initial=1,beta1_initial=1,method="deterministic",B=1000)

## posterior mean
apply(draws, 2, mean)
