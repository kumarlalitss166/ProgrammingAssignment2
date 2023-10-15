## Put comments here that give an overall description of what your
## functions do
# COMMENT: Writing a pair of functions that caches the inverse of a matrix

## Write a short comment describing this function
# COMMENT: This function creates a special object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {

    ## Initializing the inverse property    
    i <- NULL
    
    ## Setting the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Getting the matrix
    get <- function() {
    ## Returning the matrix
        m
    }
    
    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Getting the inverse of the matrix
    getInverse <- function() {
        ## Returning the inverse property
        i
    }
    ## Returning a list of the methods
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

## Write a short comment describing this function

# Compute the inverse of special matrix returned by 'makeCacheMatrix'. 
# And If the inverse has already been calculated (and the matrix has 
# not changed), then the 'cachesolve' should retrieve the inverse from the
# cache.

cacheSolve <- function(x, ...) {
    
    ## Returning a matrix that of the inverse of 'x'
    m <- x$getInverse()
    
    ## Returning if the inverse if its already set
    if (!is.null(m)) {
            message('getting cached data')
            return(m)
    }
    ## Getting the matrix from our object
    data <- x$get()
    
    ## Calculating the inverse using matrix multiplication
    m <- solve(data %*% data)
    
    ## Setting the inverse to the object
    x$setInverse(m)
    
    ## Returning the matrix
    m
}