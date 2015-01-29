## Programming in R, Programming Assignment 2
## (c) 2015 karlmelisman
## Thanks for your review. I appreciate your feedback!


## Function for a matrix and the (cached) inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  # used to save the inverted matrix, NULL if not cached so far
  inv <- NULL 
  
  # set-function, sets the matrix based on the argument
  set <- function(y) { 
    x <<- y 
    inv <<- NULL # invalidate cached value
  }
  
  # get-function, just returns the matrix
  get <- function() x 

  # setInverse-Function, 
  #   sets cached value, which has been calculated outside
  setInverse <- function(i) inv <<- i 
  
  # getInverse-Function, 
  #   just returns the inverted matrix (or NULL if not available)
  getInverse <- function() inv
  
  # create list of the functions und returns it to the outside world
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}



## Function returns a matrix that is the inverse of 'x', 
##   very fast, in case the results could be cached in further calls
cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  if(!is.null(m)) {  # check, if a cached value is available
    message("getting cached data")
    return(m)  # cached value found and to be returned
  }
  
  # the inverse matrix is not in the cache, so has to be calculated
  data <- x$get()   # get the matrix
  m <- solve(data, ...)  # calculate the inverse using the standard-function solve
  x$setInverse(m)  # put the calculated result in the cache
  m # return the inverted matrix 
}