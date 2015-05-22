## These functions will cache the matrix "x" in memory, invert the matrix,
## and cache the inverted matrix in memory

## This function contains a list of functions that will cache and retrieve the matrix,
## invert the matrix, and cache and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m<<- solve
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will check to see if the inverse is cached, and if so, return the cached
## value.  If the cached value is NULL, the inverse will be calculated and cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
