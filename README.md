# Mt.Stacker
<h1> A multivariate Stacking Algorithm </h1>

<b> Date: </b> 5/29/2017

<b> Authors: </b> Joshua Mayer

<b> Platform: </b> R Version 3.3.2

<b> Required packages: </b>  caret

<b> Maintainer: </b> Joshua Mayer <emph> joshua.mayer@ttu.edu </emph> 

<b> Description: </b> Multivariate stacking of multivariate models using a downhill simplex method.

<h2> Usage </h2>

<code>
my.Stacker(fit.mods, dat, vars, covar = NULL, nfold = 5, response.position){
</code>

<b> Inputs </b>

<strong> fit.mods: </strong> A list of functions to be stacked. The functions must be of the form <code> fun(dat, vars) </code> and the output of those functions must be test predictions.

<strong> dat: </strong> A list of dataframes consisting of an entry in the list labeled <it> test </it> and <it> train </it>

<strong> vars: </strong> 

<strong> mtry: </strong> An integer greater than or equal to 1. The number of variables sampled for each tree.

<strong> alpha: </strong> An number between 0 and 1. The significance level declared for feature removal.

<strong> prop.test: </strong> A number between 0 and 1. The size of the test set for the secondary test, as a proportion of the data. Default is 0.632.

<strong> response.position: </strong>  The column of which the responses are located. It could be done automatically with the Formula package, but this breaks down in high dimensions.

<h2> Details </h2> The following is the function to run the Sequential Multi Response Feature Selection (SMURFS). The function selects a subset of features of size <emph> mtry </emph> and a bootstrap sample of size <emph> n </emph>, grows a tree from those features and that bootstrap sample using the conditional inference framework (Hothornet <i> et al. </i>, 2006), then selects the features that are significant at any node of the tree. Features that are not selected are tested on a test set that is a subset of the data. Features that fail the second test are removed from consideration. After <i> ntree </i> iterations the features that survive are the selected features.

<h2> Value </h2> A list of survived covariates.

<h2> Examples </h2> 

    library(MASS)
    library(Matrix)
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


