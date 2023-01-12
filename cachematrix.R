## Put comments here that give an overall description of what your
## functions do

## Our aim in this assignment is to write a pair of functions that we
## will use to create a special object that stores a square matrix and 
## caches its inverse

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special
## “matrix” created with the above function. However, it first
## checks to see if the inverse has already been calculated. If 
## so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets it
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

## Testing the code
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
cacheSolve(my_Matrix)

## Testing with another example
my_Matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
my_Matrix$getinverse()
