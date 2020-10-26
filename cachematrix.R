## A pair of functions that cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    if(dim(x)[1] == dim(x)[2]) {
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
    } else {
        message("Error: Matrix must be square")
    }
}


## Compute the inverse of the special "matrix" returned by `makeCacheMatrix` function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` retrieves the inverse from the cache.
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
