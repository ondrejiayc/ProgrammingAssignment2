## For an unchanging huge matrix it may be benefitial to cache its inverse (that takes forever to compute) for later use.
## These two functions deal with it.

makeCacheMatrix <- function(x = matrix()) {

        ## Take and store a matrix and assign NULL as its inverse until it's been replaced by calling cacheSolve.

        inverted <- NULL 
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverted <<- solve
        getinv <- function() inverted
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

cacheSolve <- function(x, ...) {

        ## for an input list provided by makeCacheMatrix, return cached inverse, or calcualte and cache it.

        inverted <- x$getinv()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setinv(inverted)
        inverted
}
