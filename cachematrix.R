## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    m <- x
    setmtx <- function(x) {
        m <<- x;
        inv <<- NULL;
    }
    getmtx <- function() return(m);
    setinv <- function(x) inv <<- x;
    getinv <- function() return(inv);
    return(list(setmtx = setmtx, getmtx = getmtx, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("using cached data")
        return(inverse)
    }
    data <- mtx$getmtx()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
