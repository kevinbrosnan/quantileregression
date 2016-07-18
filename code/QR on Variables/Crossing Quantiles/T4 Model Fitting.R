#####               T4 Model Fitting                #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 21/02/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling T4 v Gestation initally with a linear      #
#  model and then including a BrokenStick element       #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  # T4 Model 1

  modT4_1 <- rq(T4 ~ Gestation, data=thyroid, tau=taus)
  plot.QR(modT4_1,x=Gestation, y=T4)

  # T4 Model 2
  temp <- Gestation
  for(i in 1:length(Gestation)){
    if(Gestation[i]>=25){
      temp[i] <- Gestation[i] - 25
    }else{
      temp[i] <- 0
    }
  }

  modT4_2 <- rq(T4 ~ Gestation + temp, data=thyroid, tau=taus)
  plot(y=T4, x=Gestation)

  col <- rainbow(length(taus))
  for(i in 1:length(taus)){
    curve(from=10,to=25,modT4_2$coef[1,i]+x*modT4_2$coef[2,i], add=TRUE,lty=2, col=col[i])
    curve(from=25, to=40,modT4_2$coef[1,i]+x*modT4_2$coef[2,i]+(x-25)*modT4_2$coef[3], lty=2,col=col[i],add=TRUE)
  }
  title(main="Quantile Regression:", cex=1.5)
  mtext("T4 ~ Gestation + BrokenStick(25)",cex=1)
  labels <- rep("", length(modT4_2$tau))

  for(i in 1:length(modT4_2$tau)){
    labels[i] <- paste("Tau = ", modT4_2$tau[i])
  }

  par(xpd=TRUE)
  #legend((max(Gestation)+1),max(T4, na.rm=TRUE), labels, cex=0.8, col=col, lty=2, title="Quantile",bty="n")
  