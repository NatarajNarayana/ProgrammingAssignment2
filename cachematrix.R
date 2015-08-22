## When running computations that are time consuming, it is very useful to cache the computation results.
## This caching can be used to look up at values later instead of computing them again. Maxtrix Inversion, 
## for example, is usually costly, especially when being run inside a loop. The following functions can be used 
##  compute and cache the inverse of a matrix.

## makeCacheMatrix fucntion creates a list that contains a function that does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
    matrixInverse <- NULL
    
    ## function to set the value... 
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    ## function to get the value...
    get <- function() x
    
    ## function to set the inverse value...
    setInverse <- function(inverseValue) matrixInverse <<- inverseValue
    
    ## function to get the inverse value...
    getInverse <- function() matrixInverse
    
    ## functions list...
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function below will return the inverse of the given matrix. 
## It first checks if the inverse has already been computed. If so, it gets
## the result and skips the additional computation. If not, it computes the inverse, 
## and sets the value into the cached variable via setinverse function.

## Assumptions Made: This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inverseValue = x$getInverse()
    
    # if the inverse has already been calculated
    if(!is.null(inverseValue))
    {
        # get it from the cache and skips the computation. 
        message("getting data from cache...")
        return(inverseValue)
    }
    
    # otherwise, calculates the inverse 
    matrixData = x$get()
    inverseValue = solve(matrixData, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setInverse(inverseValue)
    
    return(inverseValue)
}