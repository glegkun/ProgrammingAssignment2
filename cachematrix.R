
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setinverse=setInverse, getinverse=getInverse)

}



# We propose that matrix is always invertible
 
 
## The following function  returns the inverse of a matrix  created with
## the makeCacheMatrix function.
## If inverse has already been computed, cacheSolve retrieves it,
## If not, it computes, caches and returns the inverse
cacheSolve <- function(x, ...) {
        
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    inv <- solve(x$get())
    x$setInverse(inv)
    inv
}


