#####        TSH Model Fitting - Non-Crossing       #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 12/03/2014                                      #
#                                                       #
# Description:                                          #
#  Modelling TSH v Gestation - sqrt(TSH)~Gestation      #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  taus <- c(0.01,0.02,0.03,0.04,0.05,0.1,
            0.5,0.9,0.95,0.96,0.97,0.98,0.99)

  # TSH Model

    model <- QR(TSH^0.5 ~ Gestation, data=thyroid, tau=taus)

  # Plotting regression quantile lines

    plot.QR(model, x=Gestation, y=TSH, lambda=0.5, legend=FALSE)