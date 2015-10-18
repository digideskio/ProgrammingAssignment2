## This function creates a special matrix object that can cache its inverse
##
## It returns a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) xinv <<- solve
    getSolve <- function() xinv

    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix.
##
## If the inverse has been calculated and the matrix hasn't changed, cacheSolve
## should retrieve the inverse from the cache.
##
## It returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## First it checks whether the cache already exists in the environment
    if (!exists("cache")) {
        ## Here makeCacheMatrix function is memoized
        cache <<- makeCacheMatrix(x)
    }

    ## Then it checks whether the data input is the same as the one in the cache
    data <- cache$get()
    if (identical(x, data)) {
        message("Same data input")

        xinv <- cache$getSolve()
        if (!is.null(xinv)) {
            message("Returning cached solve")
            return(xinv)
        }
    }

    ## Otherwise set the data input and inverse result in the cache
    cache$set(x)
    data <- cache$get()

    xinv <- solve(data)
    cache$setSolve(xinv)
    xinv
}
