#####              B-Spline Function                #####
#                                                       #
# Author: Kevin Brosnan                                 #
# Date: 21/02/2014                                      #
#                                                       #
# Description:                                          #
#  Function to generate b-spline functions with the     #
#  following input parameters:                          #
#     1) x - vector of data                             #
#     2) ndx - number of divisions in the range         #  
#     3) xlr - the vector c(xl, xr)                     #
#     4) knots - knot positions (non-decreasing)        #
#     5) deg - degree of the B-spline                   #
#     6) deriv - derivative of a given order            #
#     7) outer.ok - is x be allowed outside inner knots #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

bspline <- function(x, ndx, xlr=NULL, knots=NULL, deg=3, 
                    deriv=0, outer.ok=FALSE){
  
  if(is.null(knots)){
    if(is.null(xlr)){
      xl <- min(x) - 0.01*diff(range(x))
      xr <- max(x) + 0.01*diff(range(x))
    }else{
      if(length(xlr)!=2){
        stop("when provided, xlr muxt have 2 components")
      }
      
      xl <- xlr[1]
      xr <- xlr[2]
    }
    
    dx <- (xr - xl)/ndx
    knots <- seq(xl - deg*dx, xr + deg*dx, by=dx)
  }else{
    if(length(knots)!=(ndx+1+2*deg)){
      stop("Error in the number of knots provided")
    }
  }
  
  B <- splineDesign(knots, x, ord=deg+1, derivs=rep(deriv, length(x)), outer.ok=outer.ok)
  
  return(B)
}