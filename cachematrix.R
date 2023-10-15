## Put comments here that give an overall description of what your
## functions do
# COMMENT: Writing a pair of functions that caches the inverse of a matrix

## Write a short comment describing this function
# COMMENT: This function creates a special object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() {m}
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {i}
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

## Write a short comment describing this function

# After calculating the inverse of special matrix returned by 'makeCacheMatrix'. 
# And if the inverse has already been calculated (and matrix hasn't 
# changed), then 'cachesolve' will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
            message('getting cached data')
            return(m)
    }
    data <- x$get()
    m <- solve(data %*% data)
    x$setInverse(m)
    m
}