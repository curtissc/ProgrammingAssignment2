## Together, the functions take a given matrix and cache its inverse.

## Make a matrix for which to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m<<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Check to see if the inverted matrix has been cached yet. If not, invert
## the matrix in makeCacheMatrix and cache it in the variables of that 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    row <- nrow(data)
    col <- ncol(data)
    g <- rev(data)
    m <- matrix(g, row, col)
    x$setinverse(m)
    m
}