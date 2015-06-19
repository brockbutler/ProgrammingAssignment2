## Assignment #2: Caching the Inverse of a Matrix
## Brock Butler

## This file creates a pair of functions that cache the inverse of a matrix
## to avoid the overhead of repeated computation

## makeCacheMatrix: This function creates a special "matrix" object that 
##                  can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <-function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
        ## Returns a list containing a function to ...
        ## 1. set the value of a matrix
        ## 2. get the value of a matrix
        ## 3. set the inverse of the matrix
        ## 4. get the inverse of the matrix
}


## cacheSolve: This function computes the inverse of the special "matix"
##             returned by makeCacheMatrix. If the inverse has already been 
##             calculated (and the matrix has not changed), cachesolve
##             retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv < x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
        ## Return a matrix that is the inverse of 'x'
}
