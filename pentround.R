''' 
Simple functions for rounding to the nearest increment of 5 (instead of 10)
Because this is over simple, I added some other very basic stats functions

pentroundOld <-- minimal R function calls
pentround <-- using the R 'round' function to make a short simple function
sample_dev <-- standard deviation of a sample (vs population)
findfourth <-- find the range of non-outliers from the fourth quartile (see whisker-box plots)

Nicholas Zehm
5/24/2023
'''
pentroundOld <- function(x) {
  if (x %% 5 == 0) {
    return (x)
  }
  else {
    x_int <- x%/%5
    x_remain <- (x%%5) / 5.0
    
    if (x_remain < 0.5) {
      return(x_int * 5)
    }
    else if (x_remain >= 0.5) {
      return(x_int * 5 + 5)
    }
  }
}

pentround <- function(x) {
  return (round(x/5)*5)
}

sample_dev <- function(x) {
  m <- mean(x)
  Sxx <- sum((x - m)^2)
  n1 = length(x) - 1
  v = Sxx/n1
  d = sqrt(v)
  return(d)
}

findfourth <- function(x) {
  m = median(x)
  q1 = median(x[x <= m])
  q3 = median(x[x >= m])
  q4 = q3 - q1
  o <- q4 * 1.5
  return(o)
}
