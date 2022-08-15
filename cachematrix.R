## Put comments here that give an overall description of what your
## functions do

##Byanca M. 
## Week 3 Assignment

## The pair of functions cache the inverse of a matrix.


## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setInverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get,
             setInverse = setUnverse,
             getInverse = getInverse)
}



## Write a short comment describing this function

## cacheeSolve: computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        
}
