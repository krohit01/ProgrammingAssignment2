## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    
    ## Initialize i <-NULL
    i <- NULL
    
    ##setting the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse of matrix
        i
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse : "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a inverse matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
	## m is cached inverse of matrix
    }
    
    ## Get the matrix from the object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}