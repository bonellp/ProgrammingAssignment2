## This pair of functions are used to cache the inverse of a matrix

## First, the makeCacheMatrix function:
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) {
  b <- NULL 
  ##Initializes objects a and b
  ##a will be the input matrix and b will be the inverse of a
  set <- function(y) {   
    a <<- y  
    b <<- NULL 
  }
  ##Defines the set function
  ##Assign the input argument to a in the parent environment and
  ##clears any value of b that had been cached by a prior execution
  get <- function() a
  ##retrieves a from the parent environment of makeVector()
  setinverse <- function(inverse) b <<- inverse
  getinverse <- function() b
  ##defines the setter for inverse b and
  ##defines the getter for the inverse m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}##assigns each of these functions as an element within a list() 
##and returns it to the parent environment
##Then, the cachesolve function:

##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above
cachesolve <- function(a = matrix(), ...) {
  b <- a$getinverse()
  #Initializes a and retrieve a inverse from the object passed in as the argument
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  #checks to see whether the result is NULL, if it's not
  #we have a valid, cached mean to be returned to the parent environment
  data <- a$get()
  b <- solve(data)
  a$setinverse(b)
  b
  #if it is NULL, the function gets the matrix from the input object
  #calculates the inverse using the function solve(), 
  #then set the inverse in the input object and
  #returns the value to parent environment, printing the value of the inverse.
}