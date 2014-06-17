## makeCacheMatrix and cacheSolve work together to store the inverse of the matrix in 
## an other environment so that it does not have to be recalculated every time its used.

## makeCacheMatrix creates a special "matrix" to 'set', 'get', 'setinverse', and 'getinverse'

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the mean of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
