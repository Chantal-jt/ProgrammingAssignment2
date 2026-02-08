## These functions save the inverse of a matrix so it only has to be calculated once.
## This inverse can be retrieved and reused instead of having to do the calculation again.

## The following function stores a matrix and its resulting inverse:

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function () {
                x
        }
        
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        getInverse <- function() {
                inv
        }
        
        list (
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## The next function checks for chached inverse and returns this inverse.
## If inverse can't be retrieved, the function calculates the inverse, stores the new inverse,
## and then returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
                
        if (!is.null(inv)) {
        message("Obtaining catched data")
        return(inv)
         } 
                         
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
