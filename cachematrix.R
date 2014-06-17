##---------------R Programming Assignment-2 ---------------------
##---------------By Muddsair Sharif------------------------------
##---------------muddsairsharif@gmail.com ---------------------


# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inve <<- inverse
    getinverse <- function() inve
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function will returns the inverse of the matrix. first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function normally expect that matrix is always invertible.

cacheSolve <- function(x, ...) {
    inve <- x$getinverse()
    if(!is.null(inve)) {
        message("fetching cached data.")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data)
    x$setinverse(inve)
    inve
}

## example run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## at first run there will be No cache
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## fetching from the cache in the second run
## > cacheSolve(m)
## archiving cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
