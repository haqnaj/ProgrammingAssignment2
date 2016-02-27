
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    list (
        set = function(y) {
            x <<- y
            m_inv <<- NULL
        },
        
        get = function() {
            x
        },
        
        set_inv = function(i) {
            m_inv <<- i
        },
        
        get_inv = function() {
            m_inv
        }
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inv()
    if (!is.null(i)) {
        message("got cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inv(i)
    i
}

