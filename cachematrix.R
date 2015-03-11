## makeCacheMatrix creates a list of functions and with an enclosing
## environment, returning the list of functions. Its behavior mimics that of
## a constructor for an object whose data members are a matrix x and 
## its inverse matInv, and whose methods are getMat, setMat, getInv 
## and setInv, corresponding to methods for retrieving and setting x
## and for retrieving and setting matInv.

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    setMat <- function(nextMat) {
        x <<- nextMat;
        matInv <<- NULL
    }
    
    getMat <- function()x
    
    setInv <- function(matI){
        matInv <<- matI
    }
    
    getInv <- function(){
        matInv
    }
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
    
}


## cacheSolve takes as its argument the output from makeCacheMatrix and returns
## the cached inverse if available, and other computes and caches the inverse,
## and returns the newly generated inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getInv()
    if (!is.null(inv)) {
        message("returning cached inv") #Included for ease of testing caching feature
        return(inv)
    }
    
    mat <- x$getMat()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
