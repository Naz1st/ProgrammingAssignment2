## This is coursera programming assignment №2
## There are 2 functions for caching and inversing matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinv <- function(solve) cache <<- solve
        getinv <- function() cache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getinv()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setinv(cache)
        cache
}
## Note it supposed to return cached data only if you use special "matrix", and if assigned to variable?
## Example os usage:
## generate matrix (or use existing) ***************************************** z <- matrix(rnorm(16), nrow = 4)
## create special matrix             ***************************************** sz <- makeCacheMatrix(z)
## return inverse                    ***************************************** cacheSolve(sz)
## all future inverses will begin with words getting cached data ************* cacheSolve(sz)
