## Put comments here that give an overall description of what your
## functions do

## Given an invertible matrix, this function creates a 
## list containing a function to set the value of the 
## input matrix (IM), get the value of the IM, set the value 
## of the IM's inverse and get the value of the IM's inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object 
        ## that can cache its inverse
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with 
## the above function. It first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
