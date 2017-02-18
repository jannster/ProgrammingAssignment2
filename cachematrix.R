## Put comments here that give an overall description of what your
## functions do

## Same as makeVector from exercise description
## but adapted naming for new task; computation itself is identical

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(mNewInverse) mInverse <<- mNewInverse
        getInverse <- function() mInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## Same as cachemean from exercise description
## but adapted naming for new task; computation adapted for matrix inversion

cacheSolve <- function(x, ...) {
	# check, if inverse is already in cache; if yes, take it and exit
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                message("getting cached data")
                return(mInverse)
        }
	# if not, retrieve data from cache, compute 
        mMatrix <- x$get()
        mInverse <- solve(mMatrix, ...)
        x$setInverse(mInverse)
        mInverse
}