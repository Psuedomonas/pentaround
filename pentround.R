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

# Round to the nearest 5 without calling R functions (does use built in operators)
# Use on vector of length 1 or more
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

# Simple concise implimentation of round to nearest 5
# Use on vector of length 1 or more
pentround <- function(x) {
  return (round(x/5)*5)
}

# Get the standard deviation of a sample (vs population)
# Use on a vector, probably more than 1 or 2 in length...
sample_dev <- function(x) {
  m <- mean(x)
  Sxx <- sum((x - m)^2)
  n1 = length(x) - 1
  v = Sxx/n1
  d = sqrt(v)
  return(d)
}

# find the range of non-outliers using median and fourth quartile
# Use on a vector, probably more than 1 or 2 in length...
findfourth <- function(x) {
  m = median(x)
  q1 = median(x[x <= m])
  q3 = median(x[x >= m])
  q4 = q3 - q1
  o <- q4 * 1.5
  return(o)
}
