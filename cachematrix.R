## Put comments here that give an overall description of what your
## functions do
#######    These functions make use of the superassignment operator in order to 
#######    take advantage of lexical scoping in R. The objective is to create 
#######    cached data. 

## Write a short comment describing this function
#######    This function creates the list containing functions in order to 
#######    set the value of the matrix, get its value, set the value of its 
#######    inverse, and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#######    This function takes as input the list above, and returns the inverse,
#######    either from the cache (basically the list returned makeCacheMatrix()) 
#######    or from scratch. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        message("Calculating from scratch...")
        inv
}
