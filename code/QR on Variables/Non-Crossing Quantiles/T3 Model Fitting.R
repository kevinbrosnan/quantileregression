#####        T3 Model Fitting - Non-Crossing        #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 12/03/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling T3 v Gestation with a quadratic model with #
#  non-crossing constraints applied                     #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #


  taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,
            0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  model <- QR(T3 ~ Gestation + I(Gestation^2), data=thyroid, tau=taus)

  # Plotting the regression quantile curves
    
    par(pty="s")
    plot(x=Gestation, y=T3, ylim=c(2.9,6.5)) 

    col <- rainbow(length(taus))
    for(i in 1:length(taus)){
      curve(model$coef[1,i]+x*model$coef[2,i]+(x^2)*model$coef[3,i],
        add=TRUE,lty=2, col=col[i])
    }

    title(main="Quantile Regression:", cex=1.5)
    subtitle <- bquote(T3*" ~ "*Gestation + Gestation^2)
    mtext(subtitle,cex=1)

    #labels <- rep("", length(model$tau))

    #for(i in 1:length(model$tau)){
      #labels[i] <- paste("Tau = ", model$tau[i])
    #}

    par(xpd=TRUE)
    #legend((max(Gestation)+1),max(T3, na.rm=TRUE), 
         #labels, cex=0.8, col=col, lty=2, title="Quantile",bty="n")
