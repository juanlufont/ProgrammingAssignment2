## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a regular matrix "x" and stores it
## together with an attribute to store cached inverse matrix 
## and several functions to allow access and modify the values
## of "x" values and cached inverse memory

makeCacheMatrix <- function(x = matrix()) {
        ## stores the inverse matrix for x matrix
		inverse <- NULL

		# stores y as the new matrix value
		# and removes any previous cached inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv.value) inverse <<- inv.value
        getinverse <- function() inverse
		# building special list with the functions that
		# allow to modify and get values from the special matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve receives a parameter "x" built using the function
## makeCacheMatrix. The function will check if "x" has already
## an inverse matrix stored in "inverse" attribute. If so, it
## just returns such matrix, otherwise, calculates it and also
## stores it in the cache attribute for future usage

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
		# getting original matrix
        data <- x$get()
		# calculating inverse using "solve"
        m.inverse <- solve(data, ...)
		# storing in cache inverse for future usage
        x$setinverse(m.inverse)
		# returning inverse matrix
        m.inverse
}
