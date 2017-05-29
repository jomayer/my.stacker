# MV Stacker
<h1> A multivariate Stacking Algorithm </h1>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
</script>

<b> Date: </b> 5/29/2017

<b> Authors: </b> Joshua Mayer

<b> Platform: </b> R Version 3.3.2

<b> Required packages: </b>  caret

<b> Maintainer: </b> Joshua Mayer <emph> joshua.mayer@ttu.edu </emph> 

<b> Description: </b> Multivariate stacking of multivariate models using a downhill simplex method.

<h2> Usage </h2>

<code>
MV.Stacker(fit.mods, dat, vars, covar = NULL, nfold = 5, response.position){
</code>

<b> Inputs </b>

<strong> fit.mods: </strong> A list of functions to be stacked. The functions must be of the form <code> fun(dat, vars) </code> and the output of those functions must be test predictions.

<strong> dat: </strong> A list of dataframes consisting of an entry in the list labeled <i> test </i> and <i> train </i>

<strong> vars: </strong> A list of additional arguments for fit.mods

<strong> covar: </strong> An optional list of covariance matricies of <code> length </code> equal to <code>nrow(dat$train) </code>  to use in the stacking algorithm. The default is to use <code> cov(dat$train) </code>

<strong> nfold: </strong> The number of folds used in stacking. The default is 5.

<strong> response.position: </strong> A vector of integers detailing which columns the response is located in the dataframe.


<h2> Details </h2> The following function is a multivariate stacking function to stack multiple multivariate models for prediction. The resulting model is $ \sum_{i=1}^n w_i f_i(x) $

<h2> Value </h2> A list of survived covariates.

<h2> Examples </h2> 

    library(MASS)
    library(Matrix)
    library(partykit)
    library(glmnet)
    set.seed(100)
    beta <- c(runif(50,1,3), rep(0,950))  
    sigma.y <- matrix(c(1,0.7,0.7,0.7,1,0.7,0.7,0.7,1), nrow = 3,  byrow = F)
    omega <- function(n)
    {
    my.mat <- matrix(0.7, n, n)
    diag(my.mat) <- rep(1,n)
    return(my.mat)
    }
    sigma.x <- bdiag(omega(50), diag(1,950))
    set.seed(100)    
    xx <- mvrnorm(200, rep(0,1000), sigma.x)
    means <- xx %*% beta
    set.seed(100)
    yy <- t(sapply(1:200, function(i) mvrnorm(n=1, mu = rep(means[i,],3), Sigma = sigma.y)))
    dat <- as.data.frame(cbind(xx,yy))
    set.seed(100)
    var.select <- SMuRFS(formula = V1001 + V1002 + V1003 ~., data = dat, ntree = 500, mtry = 8,
    alpha = 0.05, prop.test = .632, response.position = c(1001,1002,1003))

################################################################
################################################################


