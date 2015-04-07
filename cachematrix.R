## My functions (makeCacheMatrix and cacheSolve) are able to cache the
## value of the inverse of a matrix, so that if needed again, it can be
## looked up in the cache rather than recomputed. 

## The function makeCacheMatrix creates a special "matrix", which is really
## a list containing functions to:
## a) set the value of a matrix (set)
## b) get the value of the matrix (get)
## c) set the value of the inverse (setinv)
## d) get the value of the inverse (getinv)


makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message ("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

