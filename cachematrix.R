## The functions makeCacheMatrix and cacheSolve take a matrix, make a list of functions
## to set the matrix, get the matrix, set the inverse of thee matrix, and get the inverse 
## of the matrix. Once the list is created, it can be called from he cacheSolve funcion, which
## will take this special list, calculate and cache the inverse of the matrix.

## makeCacheMatrix makes a special variable that sets funcions to set and get the cached
## matrix and inverse of the matrix. This cacheMatrix variable can be called by the function
## cacheSolve to calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## cacheSolve takes as an argument the resulting cacheMatrix from makeCacheMatrix
## and determines the inverse matrix using solve(), and caches it by calling the
## function setinv from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinv()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
    }
