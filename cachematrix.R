
## Programming Assignment 2: Two functions that cache the inverse of a matrix

## make a special matrix object that can cache the inverse matrix
makeCacheMatrix <- function(mx = matrix()) {

        ## initialize the inverse 
        ix <- NULL
        
        ## setter for the matrix
        set <- function(matrix) {
                ## <<- assigns value to an object in an environment that is different from the current environment
                mx <<- matrix
                ix <<- NULL
        }
        
        ## getter for the matrix
        get <- function() {
                ## return matrix mx
                mx
        }
        
        ## setter for the inverse of the matrix
        setInverse <- function(inverse) {
                ix <<- inverse
        }
        
        ## getter for the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                ix
        }
        
        ## return a list of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## (1) compute the inverse of the special matrix returned by "makeCacheMatrix". 
## (2) if the inverse has already been calculated (and the matrix is unchanged)
##     then the "cachesolve" retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mx <- x$getInverse()
        ## simply return the inverse if it is already set
        if( !is.null(mx) ) {
                message("getting cached data")
                return(mx)
        }
        
        ## get the matrix from getter
        data <- x$get()
        ## calculate inverse...note solve() needs LAPACK from http://www.netlib.org/lapack
        mx <- solve(data) %*% data
        ## set the inverse to the mx object
        x$setInverse(mx)
        ## return the matrix
        mx
        
        
}
