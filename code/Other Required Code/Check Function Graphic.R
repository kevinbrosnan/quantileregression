rho <- function(x,tau){
  x*(tau - (x<0))
}

x <- seq(-10,10,0.01)

par(pty="s")
plot(x, rho(x,0.5), type="l", col="grey", lty=2, lwd=2, ylim=c(0,8), axes=FALSE, xlab="", ylab="")
curve(rho(x,0.2),add=TRUE, col="blue", lty=2, lwd=2)
curve(rho(x,0.3),add=TRUE, col="red", lty=2, lwd=2)
arrows(x0=0,y0=0,x1=0,y1=8,lty=1, col="black")
arrows(x0=0,y0=0,x1=10,y1=0, lty=1, col="black")
arrows(x0=0,y0=0,x1=-10,y1=0, lty=1, col="black")
text(x=1.4, y=7, expression(rho[tau](u)))

title("Check Function")
mtext(expression(rho[tau](u)*" = u("*tau*" - (u<0))"))
text(x=8,y=1,expression(tau*" = 0.2"), col="blue")
text(x=-9,y=5.2,expression(tau*" = 0.3"), col="red")
text(x=4,y=3.2,expression(tau*" = 0.5"), col="grey")
