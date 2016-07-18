#####        T4 Model Fitting - Non-Crossing        #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 12/03/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling T4 v Gestation using a broken stick model  #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,
              0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  # Setting up the Broken Stick element of the model
    
    temp <- Gestation
    for(i in 1:length(Gestation)){
      if(Gestation[i]>=25){
        temp[i] <- Gestation[i] - 25
      }else{
        temp[i] <- 0
      }
    }
  
  # Modelling T4 ~ Gestation + max(0, Gestation-25)
    
    model <- QR(T4 ~ Gestation + temp, data=thyroid, tau=taus)
    
  # Plotting Quantile Regression lines
    
    par(pty="s")
    plot(y=T4, x=Gestation)
  
    col <- rainbow(length(taus))
    for(i in 1:length(taus)){
      curve(from=10,to=25,model$coef[1,i]+x*model$coef[2,i], 
                      add=TRUE,lty=2, col=col[i])
      curve(from=25, to=40,model$coef[1,i]+x*model$coef[2,i]+
                    (x-25)*model$coef[3], lty=2,col=col[i],add=TRUE)
    }
    
    title(main="Quantile Regression:", cex=1.5)
    mtext("T4 ~ Gestation + BrokenStick(25)",cex=1)
    labels <- rep("", length(model$tau))
  
    #for(i in 1:length(model$tau)){
     # labels[i] <- paste("Tau = ", model$tau[i])
    #}
  
    par(xpd=TRUE)
#    legend((max(Gestation)+1),max(T4, na.rm=TRUE), labels, cex=0.8, 
 #                               col=col, lty=2, title="Quantile",bty="n")
