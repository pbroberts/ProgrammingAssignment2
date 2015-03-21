## These functions take an input matrix, solves the matrix inverse,
## and caches the inverse for quick retrieval later; they assume an 
## invertible matrix is provided as input

## converts the matrix into an object which support caching
## takes a matrix as input
makeCacheMatrix <- function(x=NULL) { 
        
        #set initial to null
        cachedInverse <- NULL #matrix()
        
        ## provide a method to set the cached solved matrix 
        ## in the parent environment
        setMatrix <- function(y) {
                x <<- y
                cachedInverse <<- NULL #matrix()
        }
        
        getMatrix <- function() x
        setInverse <- function(passedInSlvdMtrx) {
                cachedInverse <<- passedInSlvdMtrx
        }
        
        # provide a method to retrieve the cached solved matrix
        getInverse <- function() cachedInverse 
        
        #return the get and set functions 
        list(getMatrix = getMatrix,
             setMatrix = setMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## takes a makeCacheMatrix object input, creates and stores inverse on first call
## returns cached inverse on subsequent calls
cacheSolve <- function(x, ...) {
        
        # set local variable to cached inverse
        cachedInverse <- x$getInverse()
        message("Checking cache for inverse...")
        
        # test to see if cached inverse exists 
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                }
        # if cache doesn't exist, create it
        else {
                message("creating new cache...")
                data <-x$getMatrix()
                cachedInverse <- solve(data)
                x$setInverse(cachedInverse)
        }
        
        print(cachedInverse)
}
