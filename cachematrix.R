## Together, these functions create a special object that stores a matrix and caches its inverse.

## This function returns a list containing functions that set/get the value of the matrix
## and set/get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first determines whether the inverse has already been cached. If so,
## it will return the inverse from the cache. Otherwise, it will calculate the inverse
## of the data and set the value in the cache.

cacheSolve <- function(x, ...) {
        
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}