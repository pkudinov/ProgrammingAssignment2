## These two functions helps avoid recalculation of matrix inverse
## with a help of caching. One can create a special matrix object via matObj <- makeCacheMatrix(myMatrix)
## and use cacheSolve(matObj) to get inverse of myMatrix. This inverse is computed only once.
## The object matObj has get and set methods for accessing source matrix and setInverse, getInverse matrix for
## accessing cached inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
