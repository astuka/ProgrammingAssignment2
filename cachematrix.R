## makeCacheMatrix() stores a computed inverted matrix to short-term memory. cacheSolve() 
## will solve for an inverted matrix, while first seeing if such a matrix is already in memory.

##  Logic for caching an inverted matrix in memory via the "inv" variable.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Solves for an inverted matrix, then sends it to the cache. However, if the matrix had
## already been inverted, it simply grabs it from the stored "inv" variable.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
