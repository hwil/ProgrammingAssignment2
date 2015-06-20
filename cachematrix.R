## makeCacheMatrix will create a special "matrix" object that can cache its inverse.  
## Then cacheSolve will calculate the inverse of the special "matrix".
## If the matrix inverse has already been calculated (and the matrix has not changed)then the cachesolve should
## retrieve the inverse from the cache.
 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated, cacheSolve should retrieve it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}