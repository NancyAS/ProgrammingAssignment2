
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# makeCacheMatrix will create a list with a function to
    # a. set the value of the matrix
    # b. get the value of the matrix
    # c. set the value of inverse of the matrix
    # d. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        
}
    
    
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}

# This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has
# already been calculated, then `cacheSolve` should retrieve the inverse.
# This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data.")
        return(inverse)
        
}
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
    
}
