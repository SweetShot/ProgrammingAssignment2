## This function create new datatype cacheMatrix 
## new data type is contructed using matrix as arg
## it has 4 functions setmtx , getmtx , setinv , getinv 
## it returns list of 4 functions

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


## This function first checks if inverse of matrix is calculated
## If yes then it returns that ans
## Else it calculates inverse of matrix 
## saves inverse in cacheMatrix 
## and returns inverse

cacheSolve <- function(x, ...) {

  if(!is.null(x$getinv())) {
    message("using cached data")
    return(x$getinv())
  } 
  x$setinv(solve(x$getmtx()))
  return(x$getinv())
}
