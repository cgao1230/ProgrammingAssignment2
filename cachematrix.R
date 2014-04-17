## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation, so the following two
## functions are written for caching the inverse of the matrix rather than 
## computer it repeatedly
## Write a short comment describing this function
## This function, makeCacheMatrix creates a special "Matrix", which is really
## a list containing a function to do the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL #the inverse of the matrix
    set <- function(y) {
        ## set the value of the matrix
        x <<- y
        inverse <<- NULL
    }
    get <- function() x ## either set the matrix as input
    ## set the inverse of the matrix
    set_inverse <- function(inverse_input) inverse <<- inverse_input
    get_inverse <- function() inverse
    ## output the "matrix" (list)
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

## Write a short comment describing this function
## This function access the inverse of the matrix if it has been calculate
## otherwise calculate it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse() ## try to get inverse of matrix
    if(!is.null(m)) { ## if it is not null, return it
        message("getting cached data")
        return(m)
    }
    data <- x$get() ## get the real matrix
    m <- solve(data, ...) ## calculate the inverse of the matrix
    x$set_inverse(m) ## set the inverse into list
    m
}
