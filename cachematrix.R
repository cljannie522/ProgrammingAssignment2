
## a series of functions that cache the inverse of a matrix

# This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(matrix) {
        x<<- matrix
        invM <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invM <<- inverse
    getinv <- function() invM
    list(set=set,get=get,
         setinv = setinv,
         getinv = getinv)
}



# This function computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$getinv()
    if(!is.null(x)) {
        message("getting cached data")
        return(x)
    }
    data <- x$get()
    inv <- solve(data) %*% data
    x$setinv(x)
    x
}
