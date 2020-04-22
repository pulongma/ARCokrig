#library(Rcpp)
#library(RcppEigen)
#library(RcppArmadillo)


Funcc = function(x){
  return(0.5*(6*x-2)^2*sin(12*x-4)+10*(x-0.5)-5)
}

Funcf = function(x){
  z1 = Funcc(x)
  z2 = 2*z1-20*x+20 + sin(10*cos(5*x))
  return(z2)
}

#####################################################################
###### Nested design 
#####################################################################
#--- Data
Dc <- seq(-1,1,0.1)
indDf <- c(1, 3, 6, 8, 10, 13, 17, 21)
zc <- Funcc(Dc)
Df <- Dc[indDf]
zf <- Funcf(Df)

input.new = as.matrix(seq(-1,1,length.out=200))

#out = ARCokrig(formula=list(~1,~1+x1), output=list(c(zc), c(zf)),
#               input=list(as.matrix(Dc), as.matrix(Df)),
#               param=list(0.4, 0.2), cov.model="matern_5_2")
