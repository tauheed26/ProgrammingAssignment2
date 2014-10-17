## cachematrix.R contain the following two functions:
## (1) makeCacheMatrix - defines the getter and setter functions for the 
##     original and inverted matrix
## (2) cacheSolve - calculates, caches and returns the inverted matrix


###############################################################################
##
## makeCacheMatrix function takes in a square invertable matrix and defines the 
## following functions:
## --set the value of the matrix
## --get the value of the matrix
## --set the value of the inverse
## --get the value of the inverse
##
###############################################################################
makeCacheMatrix <- function(x = matrix()) {

        ## initialize invMatrix that holds the inverted matrix
        invMatrix <- NULL       

        ## Setter function that sets the matrix and re-sets invMatrix
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }

        ## Getter function that returns the matrix
        get <- function() x

        ## Setter function that sets inverted matrix
        setinverse <- function(im) invMatrix <<- im

        ## Getter function that returns the inverted matrix
        getinverse <- function() invMatrix

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


###############################################################################
##
## cacheSolve function takes in a square invertible matrix as a function
## parameter and returns the inverted matrix if already cached. Otherwise it
## first calculates the inverse, sets the cache and then returns the inveted
## matrix
##
###############################################################################
cacheSolve <- function(x, ...) {
        
        ## Fetch the inverted matrix from cache
        invMatrix <- x$getinverse()

        ## If data was cached, return
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }

        ## The case when data was not already cached
        ## Fetch the matrix
        data <- x$get()

        ## Calculate the inverted matrix using 'solve()'
        invMatrix <- solve(data)

        ## Cache the data
        x$setinverse(invMatrix)

        ## Return the data
        invMatrix
}

