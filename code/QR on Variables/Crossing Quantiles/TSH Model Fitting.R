#####             TSH Model Fitting                 #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 21/02/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling TSH v Gestation                            #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,
                0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  # TSH Model

  modTSH <- rq(TSH^0.5 ~ Gestation, 
                            data=thyroid, tau=taus)
  plot.QR(modTSH,x=Gestation, y=TSH, lambda=0.5, legend=FALSE)
