## makeCacheMatrix creates a special matrix object that holds the actual matrix as a member variable and inverse
## of it as another member variable and provides getter/setter methods. Simply put, makeCacheMatrix is an object 
## encapsulating the matrix and the inverse.

## Parameter: x - Matrix to be encapsulated - the inverse of which should be cached.
## Return the special matrix that encapsulates 'x' and its inverse

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


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix. It first checks if there is a cached 
## version of inverse of the matrix. If there exists a cached copy, it returns the same; Otherwise, it calculates
## the inverse and stores the inverse in the special matrix.

## Parameter: x - Matrix object obtained by makeCacheMatrix method
## Return a matrix that is the inverse of 'x'

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

## some test code of a simple 2x2 matrix
gg <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2, byrow=TRUE))
cacheSolve(gg)  ## this one will cache (first time for this matrix)
cacheSolve(gg)  ## this one will USE cache
