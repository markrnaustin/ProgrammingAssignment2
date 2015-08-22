## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    
    ## basic setter: set matrix data value
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## basic getter : return matrix data value
    get <- function() {
        x
    }
    
    ## basic setter : set the local inverted matrix
    setInverse <- function(solve) {
        s <<- solve
    }
    
    ## basic getter : return the cached inverted matrix
    getInverse <- function() {
        s
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getInverse()
    
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setInverse(s)
    s
}

gg <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2, byrow=TRUE))
cacheSolve(gg)  ## this one will cache (first time for this matrix)
cacheSolve(gg)  ## this one will USE cache
