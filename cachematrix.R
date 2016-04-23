#These functions cheks if the inverse of a given matrix has already been
#computed, if it has then it returns that value, otherwise computes it and
#saves it in the cache

## Returns a list 'l' which contains four functions to set and get the value
## of the matrix and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  l <<- list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## It takes the list returned by the above function as an argument and checks
## if the inverse of the matrix has alread been computed to return you that value
## (avoiding recomputation) or computes it.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
