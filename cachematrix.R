## The two functions in this file implement the costly matrix inversion operation by caching the matrix and its inverse
## If the matrix has not changed and the inverse has been calculated previously, the value is retrieved from cache

## This function creates a special "matrix" object that can cache its inverse.
## There are four different functions to achieve this
## set - This function saves the matrix in the cache
## get - This function retrives the matrix from the cache
## setinverse - This function saves the inverse of the matrix in the cache
## getinverse - This function retrieves the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y                                    ## The set function caches the matrix
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv    ## The inverse is cached
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)                  ## This creates the list of functions that make caching work
}


## The following function calculates the inverse of a square invertible matrix. 
## It first checks to see if the inverse is already stored in the cache.
## If so, it returns the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and saves it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    if(is.null(x$get())){            ## Checks if any matrix is in cache
        x$set(x)                     ## Save the matrix in cache
    }
    if(identical(x$get(),x)){        ## Check if passed matrix is same as cached matrix    
        inv <- x$getinverse()        ## Get the inverse from cache
        if(!is.null(inv)) {          ## Check for null
            return(inv)              ## Return the inverse
        }
        else{
            inv <- solve(x)          ## Calculate the inverse
            x$setinverse(inv)        ## Save the inverse in cache
            return(inv)              ## Return the inverse
        }
    }
    else{                            ## Passed matrix is not the same as cached matrix
        x$set(x)                     ## Save the matrix in cache
        inv <- solve(x)              ## Calculate inverse of given matrix
        x$setinverse(inv)            ## Save the inverse in cache
        return(inv)                  ## Return the inverse
    }
}
