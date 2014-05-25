## Put comments here that give an overall description of what your
## functions do

## THis finction solves for the inverse of a matrix and stores the values for later use.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   ##set locally
    set <- function(y) {    ##assigns values
        x <<- y
        m <<- NULL  #m set in parent environment
    }
    get <- function() x        ##return value of x
    setsolve <- function(solve) m <<- solve  ##solves inverse if x
    getsolve <- function() m  ##returns values of m inverse
    list(set = set, get = get,  #sets in parent so we can call in 2nd function
         setsolve = setsolve,  
         getsolve = getsolve)
}


## this funciton takes the values determined in the previous function and returns the inverse of a matrix.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()  ##retrieves value from previous function
    if(!is.null(m)) {   ##if value is not null returns below message
        message("getting cached data")
        return(m)  ##cached value is returned
    }
    data <- x$get()  ##gets values from previous function so we can do perform the calculation locally
    m <- solve(data, ...)  ##creates the local value for m
    x$setsolve(m)    ##solve m
    m         ##return m
}
