# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     x_inv <- NULL
     set <- function(y) {
          x <<- y
          x_inv <<- NULL
     }
     get <- function() x
     setinversematrix <- function(inverse) x_inv <<- inverse
     getinversematrix <- function() x_inv
     list(set = set, get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinversematrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinversematrix(m)
     m
}

## Here a sample run:

## Create and set the matrix
## >  x <- rbind(c(1, 1/2),c(1/2,1))
## > mat = makeCacheMatrix(x)
## > mat$get()
## [,1] [,2]
## [1,]  1.0  0.5
## [2,]  0.5  1.0

## Solve the matrix with no cache
## > cacheSolve(mat)
## [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333

## Retrieving cache
## > cacheSolve(mat)
## getting cached data
## [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
