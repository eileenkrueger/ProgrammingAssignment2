## The function "makeCacheMatrix" creates a special matrix that can cache it's inverse
## whereas cacheSolve computes it's inverse. If the inverse is already calculated
## cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of a special matrix. If inverse is calculated
## it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    # if the inverse has already been calculated
    if (!is.null(inv)){
      message
      return(inv)
    }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    
    return(inv)
}
