## The two function here allow you to define a matrix and 
## cache its inverse if the inverse is calculated, otherwise it will calculate again.

## The makeCacheMatix is a function contains 4 functions of setting and storing the matix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The "cacheSolve" function allows R to call the cached matrix inverse 
## or calculate it if not yet being calculated.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

