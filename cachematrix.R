## These functions are written to fulfill the requirement of second
## assignment. One function will create a matrix and cache its inverse
## value while the other function make use of that cached value or 
## calculate a new value if inverse matrix doesn't exist

## The function makeCacheMatric creates a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks if the inverse is already been calculated and return the
## if it already exists. If not then it calculates the inverse and returns that
## matrix. The input matrix should always be ivertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
