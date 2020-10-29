## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to

## 1 set the value of the vector
## 2 get the value of the vector
## 3 set the value of the mean
## 4 get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
       x <<- y
       m <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) m <<- inv
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
  
}


## Write a short comment describing this function
##cacheSolv ecalculates the inverse of the "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data, 
## and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
