#####  Quantile Regression Function - Non-Crossing  #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Adapted from: quantreg package by Roger Koenker       #
#               quantregGrowth by V. M. R. Muggeo et al #
#                                                       #  
# Date: 09/03/2014                                      #
#                                                       #
# Description:                                          #
#  1) Reading model arguments                           #
#  2) Required constants, functions and packages        #
#  3) Checking validity of inputted Tau values          #
#  4) Defining required data objects                    #
#  5) Calculating the regression quantiles              #
#  6) Tidying up the output                             #
#  7) Returning the required list                       #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


QR <- function(formula, tau=0.5, data, monotone=FALSE, lambda=0, ...){
  
#### 1) Reading in model arguments to a data frame ####

  # Accessing data if not supplied
    if(missing(data)){
      data <- environment(formula)
    }

  # Returns the call with all specified arguments named in full
    call <- match.call()
  
  # Returns the call with all specified arguments named in full
  # excluding the additional arguments passed to the function
  
    model_frame <- match.call(expand.dots=FALSE)
  
    m <- match(c("formula", "data"), names(model_frame), 0L)
    model_frame <- model_frame[c(1,m)]
    model_frame$drop.unused.levels <- TRUE
    model_frame[[1L]] <- as.name("model.frame")
    model_frame <- eval.parent(model_frame)
  
  # Defining x and y data which will be used in modelling
 
    model_terms <- attr(model_frame, "terms")
    Y <- model.response(model_frame)

    if(!is.empty.model(model_terms)){
      X <- model.matrix(model_terms, model_frame)
    }else{
      stop("Error in the Design Matrix")
    }

    B <- X
    p <- ncol(B)
    Ident <- diag(p)

#### 2) Required constants and functions ####

  # Tolerance level required
    eps <- .Machine$double.eps^(2/3)

  # Check Function required in for calculation of QR
    Rho <- function(u, tau){
      u * (tau - (u<0))
    }

  # Loading quantreg package
    if(require("quantreg")){
    }else{
      print("Trying to install quantreg")
      install.packages("quantreg",quiet=TRUE)
      if(require("quantreg")){
      }else{
        stop("Could not install quantreg")
      }
    }

  # Penalty parameter
    if(lambda>0){
      DD <- diag(p)
      B <- rbind(B, lambda*DD)
      Y <- c(Y, rep(0,nrow(DD)))
    }

#### 3) Checking validity of inputted Tau values ####
  
  # Checking value of Tau's that have been input
  
    if(length(tau)>0){
    
      # Make sure tau is in the rango 0<tau<1
      if(any(tau<0) || any(tau>1)){
        stop("Invalid values of tau input: 0<tau<1")
      }
    
      # Make sure tau value is not equal to 0
      if(any(tau==0)){
        tau[tau==0] <- eps
      }
    
      # Make sure tau value is not equal to 1
      if(any(tau==1)){
        tau[tau==1] <- 1 - eps
      }
    
      # Only keep unique values of tau
      tau <- unique(tau)
      tau <- sort(tau)
      
    }else{
      
      # Make sure tau is in the range 0<tau<1
      if(tau<0 || tau>1){
        # Temporairly function will not accept a negative value
        # Future will hopefully calculate full quantile range
        stop("Invalid values of tau input: 0<tau<1")
      }
      
      # Make sure tau is not equal to 0
      if(tau==0){
        tau <- eps
      }
      
      # Make sure tau is not equal to 1
      if(tau==1){
        tau <- 1-eps
      }
    }

#### 4) Defining required data objects ####

  # Coefficient Matrix to hold model coefficients
    coef <- matrix(0, ncol(B), length(tau))

  # Vector of rho values
    rho <- rep(0, length(tau))

  # Fitted and residual values
    fitted <- matrix(0, nrow(B), length(tau))
    resid <- matrix(0, nrow(B), length(tau))

#### 5) Calculating regression quantiles ####

  # If length(tau)=1 calculate quantile and return
    if(length(tau)==1){
      z <- rq.fit(x=X, y=Y, tau=tau)
      
      fit <- z
      fit$coefficients <- z$coefficients
      fit$residuals <- z$residuals
      fit$fitted.values <- z$fitted.values
      fit$formula <- formula
      fit$terms <- model_terms
      fit$xlevels <- .getXlevels(model_terms, model_frame)
      fit$call <- call
      fit$tau <- tau
      fit$residuals <- drop(fit$residuals)
      fit$rho <- rho
      fit$fitted.values <- drop(fit$fitted.values)
      fit$model <- model_frame
      fit$data <- data
      return(fit)
    }

  # Splitting tau values required into two groups
    start.tau <- 0.5
    pos.taus <- tau[(tau-start.tau)>0]
    n.pos.taus <- length(pos.taus)
    neg.taus <- tau[(tau-start.tau)<0]
    n.neg.taus <- length(neg.taus)  

  # Calculate tau for 0.5
    z.start <- rq.fit(x=B, y=Y, tau=start.tau)

    if(any(tau==0.5)){
      k <- which(tau==0.5)
      coef[,k] <- z.start$coefficients
      resid[,k] <- z.start$residuals
      rho[k] <- sum(Rho(z.start$residuals, tau=start.tau))
      fitted[,k] <- Y - z.start$residuals
    }

  # Calculate remaining quantiles
    
    if(n.pos.taus>0){
      
      b.start <- z.start$coef
      RR <- Ident
      rr <- b.start + eps

      for(i in 1:n.pos.taus){
        k <- which(tau==pos.taus[i])
        z <- rq.fit(x=B, y=Y, tau=pos.taus[i], method="fnc", R=RR, r=rr)
        
        coef[,k] <- z$coefficients
        resid[,k] <- z$residuals
        rho[k] <- sum(Rho(z$residuals, tau[i]))
        fitted[,k] <- Y - z$residuals
        
        rr <- coef[,k] + eps
      }
    }

    if(n.neg.taus>0){

      b.start <- z.start$coef
      neg.taus <- sort(neg.taus,TRUE)
      
      RR <- -Ident
      rr <- -b.start + eps
      
      for(i in 1:n.neg.taus){
        k <- which(tau==neg.taus[i])
        z <- rq.fit(x=B, y=Y, tau=neg.taus[i], method="fnc", R=RR, r=rr)
        
        coef[,k] <- z$coefficients
        resid[,k] <- z$residuals
        rho[k] <- sum(Rho(z$residuals, tau[i]))
        fitted[,k] <- Y - z$residuals
        
        rr <- -coef[,k] + eps
      }
    }

#### 6) Tidying up the output ####

  taulabs <- paste("Tau=", format(round(tau, 3)))
  dimnames(coef) <- list(dimnames(B)[[2]], taulabs)
  dimnames(resid) <- list(dimnames(B)[[1]], taulabs)

  fit <- z
  fit$coefficients <- coef
  fit$residuals <- resid
  fit$fitted.values <- fitted
  fit$formula <- formula
  fit$terms <- model_terms
  fit$xlevels <- .getXlevels(model_terms, model_frame)
  fit$call <- call
  fit$tau <- tau
  fit$residuals <- drop(fit$residuals)
  fit$rho <- rho
  fit$fitted.values <- drop(fit$fitted.values)
  fit$model <- model_frame
  fit$data <- data

#### 7) Returning the required list ####
  fit
}