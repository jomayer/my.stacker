
# MV Stacker

<h1> A multivariate Stacking Algorithm </h1>

<b> Date: </b> 5/29/2017

<b> Authors: </b> Joshua Mayer

<b> Platform: </b> R Version 3.3.2

<b> Required packages: </b>  caret

<b> Maintainer: </b> Joshua Mayer <emph> joshua.mayer@ttu.edu </emph> 

<b> Description: </b> Multivariate stacking of multivariate models using a downhill simplex method.

<h2> Usage </h2>

<code>
MV.Stacker(fit.mods, val.mods, dat, vars, covar = NULL, nfold = 5, prop.val.size = 0, response.position)
</code>

<b> Inputs </b>

<strong> fit.mods: </strong> A list of functions to be stacked. The functions must be of the form <code> fun(dat, vars) </code> and the output of those functions must be test predictions. dat must be a list with names train and test.

<strong> val.mods: </strong> A list of functions to be stacked. The functions must be of the form <code> fun(dat, vars) </code> and the output of those functions must be test predictions. dat must be a list with names train, val, and test.

<strong> dat: </strong> A list of dataframes consisting of an entry in the list labeled <i> test </i> and <i> train </i>

<strong> vars: </strong> A list of additional arguments for fit.mods

<strong> covar: </strong> An optional list of covariance matricies of <code> length </code> equal to <code>nrow(dat$train) </code>  to use in the stacking algorithm. The default is to use <code> cov(dat$train) </code>

<strong> nfold: </strong> The number of folds used in stacking. The default is 5.

<strong> prop.val.size: </strong> A number between 0 and 1 to represent the proportion of the training data used in validation.

<strong> response.position: </strong> A vector of integers detailing which columns the response is located in the dataframe.


<h2> Details </h2> 

The following function is a multivariate stacking function to stack multiple multivariate models for prediction. The resulting model is $\sum_{i=1}^n w_i f_i(x)$. The $w_i$'s are obtained using 

$$\underset{\arg\min}{w\in[0,1]^m; \sum w_j = 1}\sum_{i=1}^n (y_i - \sum w_j \hat{f}_j^k(x))^\top\Omega^{-1} (y_i - \sum w_j \hat{f}_j^k(x)) $$

Where each $\hat{f}_i^k(x)$ is the prediction obtained from from $k$th cross validated sample. The algorithm used in this case is downhill simplex, but BFGS is in production.

<h2> Value </h2> A list containing the weights of each model and it's associated test predictions. 

<h2> Examples </h2> 

    library(MASS)
    library(Matrix)
    library(party)
    library(glmnet)
    #Create the data
    set.seed(100)
    beta <- c(runif(100,0,1))  
    set.seed(100)    
    xx.train <- mvrnorm(200, rep(0,100), diag(1, 100))
    xx.test <- mvrnorm(200, rep(0,100), diag(1, 100))
    means.train <- xx.train %*% beta + 25 * 1 / (1 + exp(-xx.train %*% beta))
    means.test <- xx.test %*% beta + + 25 * 1 / (1 + exp(-xx.train %*% beta))
    set.seed(100)
    yy.train <- t(sapply(1:200, function(i) mvrnorm(n=1, mu = rep(means.train[i,],3), Sigma = diag(1,3))))
    yy.test <- t(sapply(1:200, function(i) mvrnorm(n=1, mu = rep(means.test[i,],3), Sigma = diag(1,3))))   
    dat.train <- as.data.frame(cbind(xx.train, yy.train))
    dat.test <- as.data.frame(cbind(xx.test, yy.test))
    dat <- list(train = dat.train, test = dat.test)
    
    ## create the functions
    
    
    
    my.enet <- function(dat, vars){
    xx.train <- as.matrix(dat$train[, 1:100])
    xx.test <- as.matrix(dat$test[, 1:100])
    yy <- as.matrix(dat$train[, 101:103])
    cvcv <- cv.glmnet(x = xx.train, y = yy, family = 'mgaussian',
         alpha = vars$alpha, intercept = FALSE)
    preds <- predict(cvcv, newx = xx.test, s = 'lambda.min')[,,1]
    return(preds)
    }
    
    my.rf <- function(dat, vars){
    my.forest <- cforest(vars$formula, 
           data = dat$train,
           control = cforest_unbiased(ntree = vars$ntree, mtry = vars$mtry ))
    preds <- predict(my.forest, dat$test, OOB = T)
    preds <- do.call(rbind, preds)
    return(preds)
     }

    ## create val functions 
    
    

    my.enet.val <- function(dat, vars){
    xx.train <- as.matrix(dat$train[, 1:100])
    xx.val <- as.matrix(dat$val[, 1:100])
    xx.test <- as.matrix(dat$test[, 1:100])
    yy.train <- as.matrix(dat$train[, 101:103])
    yy.val <- as.matrix(dat$val[,101:103])
    cvcv <- cv.glmnet(x = xx.train, y = yy.train, family = 'mgaussian',
             alpha = vars$alpha, intercept = FALSE)
     lambdas <- c(seq(0, cvcv$'lambda.min', by = 0.2), cvcv$'lambda.min')
     my.enet <- glmnet(x = xx.val, y = yy.val, family = 'mgaussian',
         alpha = vars$alpha, intercept = FALSE, lambda = lambdas)
     preds <- predict(my.enet, newx = xx.test, s = cvcv$'lambda.min')[,,1]
     return(preds)
     }

    my.rf.val <- function(dat, vars){
    my.forest <- cforest(vars$formula, 
           data = dat$val,
           control = cforest_unbiased(ntree = vars$ntree, mtry = vars$mtry ))
    preds <- predict(my.forest, dat$test, OOB = T)
    preds <- do.call(rbind, preds)
    return(preds)
     }


    fit.mods <- list(my.enet, my.rf, my.enet)
    
    val.mods <- list(my.enet.val, my.rf.val, my.enet.val)

    vars <- list(
    list(alpha = 0.2),
    list(mtry = 10, ntree = 500, formula = V101 + V102 + V103 ~.),
    list(alpha = 0)
    )

    set.seed(100)
    MV.Stacker(fit.mods, val.mods, dat, vars, covar = NULL, nfold = 5, prop.val.size = 0.2, response.position = 101:103)

################################################################
################################################################


