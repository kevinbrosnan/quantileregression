#####               T3 Model Fitting                #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 21/02/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling T3 v Gestation initally with a linear      #
#  model and then a quadratic model                     #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,
                        0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  # T3 Model 1

  modT3_1 <- QR(t3 ~ gestation, tau=taus)
  plot.QR(modT3_1,x=Gestation, y=T3)

  # T3 Model 2

  modT3_2 <- rq(T3 ~ Gestation + I(Gestation^2), data=thyroid, tau=taus)
  plot(x=Gestation, y=T3) 

  col <- rainbow(length(taus))
  for(i in 1:length(taus)){
    curve(modT3_2$coef[1,i]+x*modT3_2$coef[2,i]+(x^2)*modT3_2$coef[3,i],
                            add=TRUE,lty=2, col=col[i])
  }
  
  title(main="Quantile Regression:", cex=1.5)
  subtitle <- bquote(T3*" ~ "*Gestation + Gestation^2)
  mtext(subtitle,cex=1)
  
  labels <- rep("", length(modT3_2$tau))

  for(i in 1:length(modT3_2$tau)){
    labels[i] <- paste("Tau = ", modT3_2$tau[i])
  }

  par(xpd=TRUE)
  #legend((max(Gestation)+1),max(T3, na.rm=TRUE), 
   #      labels, cex=0.8, col=col, lty=2, title="Quantile",bty="n")

