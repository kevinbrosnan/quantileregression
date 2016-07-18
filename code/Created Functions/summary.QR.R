##### Goodness-of-Fit Test for QR Models Pseudo R^2 #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 26/02/2014                                      #
#                                                       #
# Description:                                          #
#  This function returns a value of pseudo-R^2 for the  #
#  goodness-of-fit of a QR model fitted to a dataset    #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


summary.QR <- function(model){
  
  rho <- function(u, tau){
    u*(tau-(u<0))
  }
  
  mod <- model
  modBase <- rq(mod$y~1, tau=mod$tau)
  
  nobs <- length(mod$y)
  r <- rep(0, length(mod$tau))
  aic <- rep(0, length(mod$tau))
  taus <- colnames(mod$resid)
  
  for(i in 1:length(mod$tau)){

    v0 <- (1/length(modBase$resid))*(sum(rho(modBase$resid[,taus], modBase$tau[i])))
    v1 <- (1/length(mod$resid))*(sum(rho(mod$resid[,taus[i]], mod$tau[i])))
  
    r[i] <- 1 - (v1/v0)
    aic[i] <- 2*nobs*(log(v1)) + 2*ncol(mod$x)
  }
  
  R <- as.data.frame(t(r))
  AIC <- as.data.frame(t(aic))
  
  rownames(R) <- expression(R^2)
  colnames(R) <- taus
  
  rownames(AIC) <- expression(AIC(tau))
  colnames(AIC) <- taus
  
  coef <- coef(mod)
  cat("\nCoefficients:\n")
  print(coef)
  rank <- mod$rank
  
  cat("\nPseudo R-squared:\n")
  print(R)
  
  cat("\nAIC:\n")
  print(AIC)
  
  if (is.matrix(coef)){ 
    p <- dim(coef)[1]
  }
  else{
    p <- length(coef)
  }
  
  rdf <- nobs - p
  cat("\nDegrees of freedom:", nobs, "total;", rdf, "residual\n")
}