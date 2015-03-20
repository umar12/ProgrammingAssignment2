## These functions are written to fulfill the requirement of second
## assignment. One function will create an inverse matrix and cache its
## value, while the other function makes use of that cached value or 
## calculate a new value if inverse matrix doesn't exist

## The function makeCacheMatric creates an inverse matrix object and cache it
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {    # Set value of the input matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve    # Solve for inverse matrix
        getinv <- function() inv    
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks if the inverse is already been calculated and return the
## matrix if it exists. Otherwise, it calculates the inverse and returns that
## matrix. The input matrix should always be ivertible
cacheSolve <- function(x, ...) {
    
        inv <- x$getinv()   #Get existing Inverted matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()    #Get the input Matrix
        inv <- solve(data, ...)    #Recalculate because inverse matrix doesn't exist
        x$setinv(inv)
        inv
}
