## The makeCacheMatrix function creates a list of tasks to compute the inverse of a square matrix
## The cacheSolve function takes the list created from makeCacheMatrix to compute the inverse of the original matrix

## makeCacheMatrix takes a matrix passed to it and creates a cached list of functions to
## create the inverse of the passed matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Set up matrix in cache and create list of functions to compute inverse
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes the matrix function list generated from makeCacheMatrix as the arguement
## and creates and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
}
