## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(im) invMatrix <<- im
        getinverse <- function() invMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The 'cacheSolve' takes in a matrix as a function parameter, then calculates
## and returns the inverse of the input matrix 

cacheSolve <- function(x, ...) {
        
        invMatrix <- x$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data)
        x$setinverse(invMatrix)
        invMatrix
}
