##### Plot of Data fitted QR lines in Base Graphics #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 19/02/2014                                      #
#                                                       #
# Description:                                          #
#  A scatterplot of the data is produced and the        #
#  calculated regression quantile curves are            #
#  superimposed on the plot in different colours. A     #
#  legend is provided to signify each quantile.         #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plot.QR <- function(model, x, y, lambda=1, legend=TRUE){
  
  data <- model$data
  attach(data,warn.conflicts=FALSE)
  
  depVar <- paste(model$formula[2])
  if(lambda!=1){
    length_lambda <- nchar(lambda) + 1
    depVar <- substr(depVar, 1, nchar(depVar)-length_lambda)
  }
  indepVar <- paste(model$formula[3:(length(model$formula))], sep=" + ")
  
  par(pty="s")
  
  plot(x=x, y=y, xlab=indepVar, ylab=depVar, ylim=c((min(y,na.rm=TRUE)-1),(max(y,na.rm=TRUE)+1)))
  title <- paste("Quantile Regression:")
  if(lambda!=1){
    subtitle <- bquote(.(depVar)^.(lambda)*" ~ "*.(indepVar))
  }else{
    subtitle <- bquote(.(depVar)*" ~ "*.(indepVar))
  }
  title(title, cex=1.5)
  mtext(subtitle, cex=1)
  
  colour <- rainbow(n=length(model$tau))
  for(i in 1:length(model$tau)){
    curve((model$coefficients[1,i] + x*model$coefficients[2,i])^(1/lambda),
          col=colour[i],lty=2, add=TRUE)
  }
  
  labels <- rep("", length(model$tau))
  
  for(i in 1:length(model$tau)){
    labels[i] <- paste("Tau = ", model$tau[i])
  }
  
  par(xpd=TRUE)
  if(legend==TRUE){
    legend((max(x)+1),max(y, na.rm=TRUE), labels, cex=0.8, col=colour, lty=2, title="Quantile",bty="n")
  }
}