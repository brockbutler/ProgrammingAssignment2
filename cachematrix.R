## Assignment #2: Caching the Inverse of a Matrix
## Brock Butler

## This file creates a pair of functions that cache the inverse of a matrix
## to avoid the overhead of repeated computation

## makeCacheMatrix: This function creates a special "matrix" object that 
##                  can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## function to set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## function to get the value of the matrix
        get <- function() x
        
        ##function to set the inverse of the matrix
        setinverse <-function(inverse) inv<<-inverse
        
        ##function to get the inverse of the matrix
        getinverse <- function() inv
        
        ## Returns a list containing a function to ...
        ## 1. set the value of a matrix
        ## 2. get the value of a matrix
        ## 3. set the inverse of the matrix
        ## 4. get the inverse of the matrix
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matix"
##             returned by makeCacheMatrix. If the inverse has already been 
##             calculated (and the matrix has not changed), cachesolve
##             retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        ## if inverse is not null, get the cached inverted matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inverse is null, compute the inverse
        data <- x$get()
        inv <- solve(data)
        
        ## cache the inverse for future use
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
