## Code for R Programming course Assignment 2
## This code creates two functions
## The first creates a special matrix  object that can cache its inverse
## The second computes the inverse of this matrix retrieving the result from cache if appropriate

## makeCacheMatrix fuction creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL          ## initialise the inverse matrix object (which we will call im)
  set <- function(y) {
    x <<- y      ## use super assignment operator to store x to cache
    im <<- NULL  ## use super assignment operator to store im to cache
  }
  get <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve checks if the inverse is already calculated & if so retrieves from the cache
## otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {                # inverse already calculated
    message("getting cached data")
    return(im)                      # return value from cache
  }
  # inverse is still null so need to calculate it
  data <- x$get()           # get the data
  im <- solve(data, ...)    # use solve function to calculate its inverse
  x$setinverse(im)
  im                        # return the inverse
}
