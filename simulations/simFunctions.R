# functions used in simulation

ordFun <- function(y){
  # input: array of continuous variables
  # output: array of orginal variables
  
  yCat <- array(NA, dim = dim(y))
  yCat[which(y < -2)] <- 1
  yCat[which(y>=-2 & y < -1)] <- 2
  yCat[which(y>=-1 & y < 0)] <- 3
  yCat[which(y>=0 & y < 1)] <- 4
  yCat[which(y>=1 & y < 2)] <- 5
  yCat[which(y>=2)] <- 6

  return(yCat)
}
