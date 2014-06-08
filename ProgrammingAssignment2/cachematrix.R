t give an overall description of what your
## functions do

## Creates a "matrix" object that can cahe its inverse
## obj$get(): get the matrix
## obj$set(): set the matrix cache
## obj$getInverse(): get the inverse OR inverse cache
## obj$setInverse(): set the inverse cache
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL  #if matrix change, reset inv
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of "matrix" object (return by
## makeCacheMatrix). If the inverse has also been calculated
## (and the matrix has not changed), then retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## For test
x <- matrix(c(1,2,3,4,8,6,7,8,9),nrow=3)
x2 <- matrix(c(5,2,3,4,8,6,7,8,9),nrow=3)
y <- makeCacheMatrix(x)
cacheSolve(y) # calculate inverse for 1st time
cacheSolve(y) # use the cache
y$set(x2) # reset y
cacheSolve(y) # re-calculate

