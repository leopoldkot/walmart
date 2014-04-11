getWeek <- function (date) {
  x <- as.POSIXlt(date) 
  as.numeric(strftime(x,format="%W"))
}

zScore <- function(x) {
  m <- x[[1]]
  z <- 1.0 + (x - m) / m
  #  z <- z - min(z, na.rm=TRUE)
  
  return(z) 
}

shiftVec <- function (vec, shift) {
  if (shift > 0) {
    c(rep(0, shift), vec[1:(length(vec) - shift)]) 
  } else {
    c(vec[(abs(shift)+1):length(vec)], rep(0, abs(shift))) 
  } 
}

addNoise <- function(mtx) {
  if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.00001, max = 0.0001), nrow = dim(mtx)[1])
  random.stuff + mtx
}
