## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeVector <- function(x = numeric()) {
makeCacheMatrix <- function(x = matrix()) {
  
    #m <- NULL
    storedSolvedDataMatrix <- NULL
    
    #set <- function(y) {
    set <- function(y = numeric()) {  
            #x <<- y
            dataMatrix <<- y
            #m <<- NULL
            storedSolvedDataMatrix <<- NULL
    }
    
    #get <- function() x
    get <- function() {
            return (dataMatrix)
    }
    
    #setmean <- function(mean) m <<- mean
    setSolvedDataMatrix <- function(sentSolvedDataMatrix) {
            storedSolvedDataMatrix <<- sentSolvedDataMatrix
    }
    
    #getmean <- function() m
    getSolvedDataMatrix <- function() {
            storedSolvedDataMatrix
    }
    
    #list(set = set, get = get, setmean = setmean, getmean = getmean)
    list(set = set, 
         get = get, 
         setSolvedDataMatrix = setSolvedDataMatrix, 
         getSolvedDataMatrix = getSolvedDataMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(makeCacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        localInverse <- makeCacheMatrix$getSolvedDataMatrix
        
        if(!is.null(localInverse)) {
                message("No previously cached inverse")
                return(localInverse)
        }
        else {
                localMatrix <- makeCacheMatrix$get()
                localInverse <- solve(localMatrix, ...)
                
                makeCacheMatrix$setSolvedDataMatrix(localInverse)
                return(localInverse)
        }
}
makeCacheMatrix2 <- function(x=matrix(,nrow=2,ncol=2)) {
        
        #m <- NULL
        storedSolvedDataMatrix <- x
        print(storedSolvedDataMatrix)

        #setmean <- function(mean) m <<- mean
        setfunction <- function() storedSolvedDataMatrix 
        
        #getmean <- function() m
        getfunction <- function() {
                storedSolvedDataMatrix <- storedSolvedDataMatrix
        }
        
        #list(set = set, get = get, setmean = setmean, getmean = getmean)
        list(set = setfunction, 
             get = getfunction)
}

cacheSolve2 <- function(makeCacheMatrix2, ...) {
        ## Return a matrix that is the inverse of 'x'
        localInverse <- makeCacheMatrix2$get
        
        if(!is.null(localInverse)) {
                message("No previously cached inverse")
                #return(localInverse)
        }
        else {
                print(localInverse)
        }
        message("Setting new cached value...")
        localMatrix <- makeCacheMatrix2$set()
        localInverse <- solve(localMatrix, ...)
        print(makeCacheMatrix2$get())        

}