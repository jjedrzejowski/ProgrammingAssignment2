## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes as argument a square matrix
## and performs the following functions:
## 1. set() - sets the matrix and initializes cached inverse matrix
## 2. get() - gets the matrix
## 3. setinv() - caches the inverse matrix
## 4. getinv() - gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse matrix as null
    INV <- NULL
    ## sets matrix x to what is being passed as y, sets cached inverse matrix as null
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    ## returns matrix x
    get <- function() x
    ## caches the inverse matrix passed as argument
    setinv <- function(inverse) INV <<- inverse
    ## returns cached inverse matrix
    getinv <- function() INV
    ## function returns list of four functions manipulating matrix x
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve takes as argument a list created by makeCacheMatrix,
## and returns inverse matrix
## if there exists a cached version of inverse matrix, cacheSolve returns that

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    invers <- x$getinv()
    ## if there is a cached version, return it, and give appropriate message
    if(!is.null(invers)) {
        message("getting cached inverse matrix")
        return(invers)
    }
    ## if there is no cached version, get the original square matrix
    DATA <- x$get()
    ## solve the matrix
    ## solve() without the right hand of linear system returns matrix inverse to x
    invers <- solve(DATA)
    ## cache the inverse matrix
    x$setinv(invers)
    ## return the inverse matrix
    invers
}

