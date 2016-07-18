##### Distribution Analysis: Graph + Normality Test #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 24/02/2014                                      #
#                                                       #
# Description:                                          #
#  1) Histogram                                         #
#  2) Boxplot                                           #
#  3) QQ-plots                                          #
#  4) Shapiro-Wilk Test                                 #  
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

distribution <- function(data){
  
  name <- deparse(substitute(data))
  split.screen(figs=c(2,1))
  split.screen(figs=c(1,2), screen=1)
  
  screen(3)
    # Box-Plot of Data
    titleBox <- paste("Boxplot of ", name)
    boxplot(data, main=titleBox)
  screen(4)  
    # QQ-plot of data
    qqnorm(data)
    qqline(data, col="red")
  screen(2)
    # Histogram of Data
    titleHist <- paste("Histogram of ", name)
    hist(data, xlab=name, main=titleHist)
  
  close.screen(all.screens=TRUE)
  
  normtest <- shapiro.test(data)
  return(normtest)
}