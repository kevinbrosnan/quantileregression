#####   Plot of Model Residuals for Base Graphics   #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 19/02/2014                                      #
#                                                       #
# Description:                                          #
#  1) QQ-plot                                           #
#  2) Residuals vs Fitted Values                        #
#  3) Histogram of Residuals                            #
#  4) Residuals vs Order                                #
#  5) Add in appropiate main titles                     #
#  6) Normality test of residuals                       #
#                                                       #
# Problems:                                             #
#  1) Subtitle stops working correctly when more than   #
#     one independent variable                          #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


test.resid <- function(model, tau=""){
  
  mod <- model
  
  if(tau==""){
    tau <- model$tau
  }else{
    tau <- tau
  }
  
  norm_test <- as.data.frame(cbind(tau,0,0))
  colnames(norm_test) <- c("Tau", "Shapiro Stat", "p-value")
  
  for(i in 1:length(tau)){
    
    resid <- mod$residuals[mod$tau==tau[i]]
    fit <- mod$fitted[mod$tau==tau[i]]
      
    par(mfrow=c(2,2), oma=c(0,0,5,0))
    
    # QQ-plot of residuals
    qqnorm(resid)
    qqline(resid)
    
    # Resiudals v's Fitted
    plot(resid~fit, main="Residuals vs Fitted values",
         xlab="Fitted", ylab="Residuals")
    abline(h=0, lty=2, col="red")
    
    # Histogram of Residuals
    hist(resid, xlab="Residuals", main="Histogram of Residuals")
    
    # Residuals vs Observation order
    plot(resid, type="b", main="Residuals vs Order",
         ylab="Residuals", xlab="Observation Order")
    abline(h=0, lty=2, col="red")
    
    
    title <- paste("Residual plots of", mod$formula[2]," ~ ",mod$formula[3:(dim(mod$x)[2] + 1)], collapse=" ")
    subtitle <- bquote(tau*" = "*.(tau[i]))
    title(main=title, outer=TRUE, cex=1.5)
    mtext(subtitle, outer=TRUE, cex=1)
    par(mfrow=c(1,1))
    
    # Normality tests for residuals
    n_test <- shapiro.test(resid)
    norm_test[i,2] <- n_test$statistic
    norm_test[i,3] <- n_test$p.value
  
  }
  return(norm_test)
}