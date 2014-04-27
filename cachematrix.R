##
## makeCacheMatrix()
##
## The following function creates a "special" matrix, which is  a list of  functions which performs various operations,
##
## 1. set() - sets the value of the input matrix.
## 2. get() - gets the value of the input matrix.
## 3. setinv() - sets the value of the matrix inverse.
## 4. getinv() - gets the value of the matrix inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  
  # initializing to NULL
  inv <- NULL
  
  # sets the value of the matrix with new input
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL # this becomes NULL since matrix data changes
  }
  
  # gets the value of the matrix
  get <- function(){
    x
  }
  
  # sets the inverse value 
  setinv <- function(i){
    inv <<- i
  }
  
  # gets the inverse value 
  getinv <- function(){
    inv
  }
  
  # returns a list containing all functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


##
## cacheSolve()
##
## This function calculates the inverse of the "special" matrix, created by the
## 'makeCacheMatrix' function by first checking to see if inverse has been
## calculated already. If it is present, then the value is fetched from the
## cache and all other steps are skipped, else if the inverse is NULL, it means
## the value is not in cache and the inverse of "special" matrix is calculated
## using the 'solve' function and it is set in the cache using the 'setinv' function.

cacheSolve <- function(x, ...) {
  
  # Checks if the inverse is already in the cache
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # inverse value not cached so we get the data of the "special" matrix
  data <- x$get()
  
  # computes the inverse 
  inv <- solve(data, ...)
  
  # stores new value of inverse in the cache
  x$setinv(inv)
  
  # returns new inverse matrix
  inv
  
}