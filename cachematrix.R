## The following two functions can create/use a special matrix object that can 
## cache its inverse so it doesn't have to be recalculated again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## if set is called, which means the value of x is changed, then reset m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## returns the matrix itself
    get <- function() x
    
    ## set m in the upper level of environment (in function makeCacheMatrix)
    setinv <- function(inv) m <<- inv
    
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    ## if m is not NULL, then the inverse has already been calculated before
    ## and x hasn't changed (set hasn't been called)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise, calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## sample output
# > a <- matrix(c(1:4), 2, 2)
# > b <- makeCacheMatrix(a)
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5