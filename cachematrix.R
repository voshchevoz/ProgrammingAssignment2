## This functions are aimed to cache potentially time-consuming calculation of
## inverse matrix in an environment where inversion is executed repeatedly.
## If the contents of a matrix are not changing, the inverse matrix is cached
## so that when it is required again, it can be looked up in the cache
## rather than recomputed.

## How to test:
## testm <- matrix(sample(1:100, 640000, replace = TRUE), nrow = 800, ncol = 800)
## cachedm <- makeCacheMatrix(testm)
## invm <- cacheSolve(cachedm)
## invm <- cacheSolve(cachedm) (should print "getting cached data")
## diagm  <- diag(nrow = 800, ncol = 800)
## all.equal(diagm, testm %*% res) (should print TRUE)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x

    setinv <- function(solve) inv <<- solve

    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve retrieves the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
