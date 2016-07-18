#####   Box-Cox Power Transformation Calculation    #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 19/02/2014                                      #
#                                                       #
# Description:                                          #
#  1) Produce a plot of lambda v's log-likelihood       #
#  2) Return the exact value of lambda to the user      #
#  3) Return suggested values of lambda to user         #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

box.cox <- function(formula, plotit=TRUE){
  
  # Checking the class of the formula argument passed to the function
    
    if(class(formula)=="lm"){
      formula <- update(object, y=TRUE, qr=TRUE)
    }else if(class(formula)=="formula"){
      formula <- lm(formula, y=TRUE, qr=TRUE)    
    }else{
      stop("Formula must be of class formula or of class lm")
    }
    
    y <- formula$y
    xqr <- formula$qr
  
  # Make sure response variable y is positive  
  
    if(any(y<=0)){
      stop("Response variable must be positive")
    }
  
  # Box-Cox Algorithm
  
    lambda <- seq(-2,2,length=length(y))
    eps <- 0.02
  
    n <- length(y)
    y <- y/exp(mean(log(y)))
    logy <- log(y)
    
    xl <- loglik <- as.vector(lambda)
    m <- length(xl)
  
    for(i in 1:m){
      if(abs(la <- xl[i]) > eps){
        yt <- (y^la - 1)/la
      }else{
        yt <- logy*(1 + (la*logy)/2*(1 + (la*logy)/3*(1 + (la*logy)/4)))
      }
      loglik[i] <- -n/2 * log(sum(qr.resid(xqr, yt)^2))
    }
  
  # Calculating maximum values of lambda and log-likelihood
    
    max_index <- which(loglik==max(loglik))
    Loglik_max <- loglik[max_index]
    lambda_max <- xl[max_index]
 
  # Plotting log-likelihood vs Lambda
  
    if(plotit){
      
      lim <- Loglik_max - qchisq(19/20, 1)/2
      
      ind <- range((1:m)[loglik>lim])
      xlim <- c(0,0)
      
      if(loglik[1] < lim){
        i <- ind[1]
        xlim[1] <- xl[i-1] + ((lim - loglik[i-1])*
                                (xl[i] - xl[i-1]))/(loglik[i] - loglik[i-1]) 
      }
      
      if(loglik[m] < lim){
        i <- ind[2]
        xlim[2] <- xl[i-1] + ((lim - loglik[i-1])*
                                (xl[i] - xl[i-1]))/(loglik[i] - loglik[i-1]) 
      }
      
      xlim <- sort(xlim)
      
      xlower <- which(xl >= xlim[1]-0.01)
      xupper <- which(xl >= xlim[2]+0.01)
      
      xlower <- xlower[1]
      xupper <- xupper[1]
      
      dev.hold()
      on.exit(dev.flush())
      
      plot(x=xl[xlower:xupper], y=loglik[xlower:xupper], xlab=expression(lambda), 
           ylab="log-Likelihood",type="l",  xlim=c(xlim[1]-0.01,xlim[2]+0.01))
      
      title(main="Box-Cox Transformation")
      
      plims <- par("usr")
      scal <- (1/4 * (plims[4]-plims[3]))/par("pin")[2]
      
      abline(h=lim, lty=3)
      text <- bquote(lambda*"="*.(round(lambda_max,2)))
      text(lambda_max, Loglik_max - scal, text, col="red")
      
      
      y0 <- plims[3]
      
      if(max_index>1 && max_index<m){
        segments(lambda_max, y0, lambda_max, Loglik_max, lty=3)
      }
      
      segments(xlim[1], y0, xlim[1], lim, lty=3)
      segments(xlim[2], y0, xlim[2], lim, lty=3)
    }
  
  # Outputting best suggestion for lambda
    
    lambda_suggestion <- FALSE
  
    if(abs(lambda_max)>0.4 && abs(lambda_max)<0.6){
      lambda_suggestion <- TRUE
      if(lambda_max<0){
        lambda_sug <- "1/sqrt(y)"
      }else{
        lambda_sug <- "sqrt(y)"
      }
    }else if(abs(lambda_max)>0.9 && abs(lambda_max)<1.1){
      lambda_suggestion <- TRUE
      if(lambda_max<0){
        lambda_sug <- "1/y"
      }else{
        lambda_sug <- "y"
      }
    }else if(abs(lambda_max)<0.1){
      lambda_suggestion <- TRUE
      lambda_sug <- "ln(y)"
    }
  
    if(lambda_suggestion){
      out <- sprintf("The optimal value of lambda to use in transforming the supplied data is lambda = %.2f. However a %s transformation would be easier to interpret and apply", lambda_max, lambda_sug)
    } else{
      out <- sprintf("The optimal value of lambda to use in transforming the supplied data is lambda = %.2f", lambda_max)
    }
    
  # Returning final values
    print(out)
    invisible(list(x=xl, y=loglik, lambda=lambda_max))
}