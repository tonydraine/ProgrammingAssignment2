## R Programming Assignment 2: Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. This assignment involves writing a pair of functions that 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cacheinverse <- NULL
   
    ## Function sets new matrix via y argument and uses <<- to assign values to objects 
    ## in the global environement. cacheinverse is set to NULL because the inverse 
    ## has not been calcualted for the new matrix yet.  
    set <- function(y){
        x <<- y
        cacheinverse <<- NULL
    }
    
    ## Function returns matrix
    get <- function() x
    
    ## Function assigns inverse value to the cacheinverse object
    setinverse <- function(inverse) cacheinverse <<- inverse
    
    ## Function returns inverse of matrix
    getinverse <- function() cacheinverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Local object is assigned the current cached inverse value
    cacheinverse <- x$getinverse()
    
    ## If cacheinverse is not null then it is returned without the need to recalculate 
    ## inverse of matrix.
    if(!is.null(cacheinverse)){
        message("Cached data")
        return(cacheinverse)
    }
    
    ## Local object is assigned the current matrix object.
    currentmatrix <- x$get()
    
    ## Solve function computes inverse of current matrix
    cacheinverse <- solve(currentmatrix, ...)
    
    ## Function assigns inverse value to the cacheinverse object
    x$setinverse(cacheinverse)
    
    ## Returns cached inverse value.
    cacheinverse
}

