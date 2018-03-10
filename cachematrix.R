## makeCacheMatrix:This function is used to create a special object that stores a matrix and caxhes its inverse. 
## cacheSolve:This function computes the inverse of the special "matirx" returned by makeCacheMatrix above. If the 
## inverse has already been calculated and matrix is also the same, then cacheSolve will retrieve the inverse from the cache.


## This function is used to create a special object that stores a matrix and caxhes its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set = set, 
          get = get,
          setinverse = setinverse, 
          getinverse = getinverse )

}

## This function computes the inverse of the special "matirx" returned by makeCacheMatrix above. If the inverse has already
## been calculated and matrix is also the same, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("GETTING CAcHED DATA")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
