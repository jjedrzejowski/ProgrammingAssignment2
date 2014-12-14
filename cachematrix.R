## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) INV <<- inverse
    getinv <- function() INV
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    INV <- x$getinv()
    if(!is.null(INV)) {
        message("getting cached inverse matrix")
        return(INV)
    }
    DATA <- x$get()
    INV <- solve(DATA)
    x$setinv(INV)
    INV
}

