## makeCacheMatrix :
## This function creates a special "matrix" object that can cache the most recent inverse matrix. It returns a set of functions that do the following:
##  Set the matrix value,  Get the matrix value, Set the inverse value, Get the inverse value.


makeCacheMatrix <- function(x = matrix()) {
	 inverseMatrix <- NULL ## delete previous inverse matrix
	 
	    ## to set matrix with matrix input
        set <- function(y) {
                x <<- y 
                inverseMatrix <<- NULL
        }
        
        ## to get matrix
        get <- function() x
        
        ## set inverse
        setinverse <- function(inverse) inverseMatrix <<- inverse
        
        ## get inverse
        getinverse <- function() inverseMatrix
        
        ## return the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve
## This function yields the inverse matrix made by makeCacheMatrix. 
##If this has been done previously and matrix is not modified, the cached inverse is then returned.

cacheSolve <- function(x, ...) {
        ## If inverse was previously computed, return this inverse matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        
        
        ## if inverse was not computed, calculate it now... 
        data <- x$get()
        inverseMatrix <- solve(data)
        
        
        ## Then cache the new inverse and return it
        x$setinverse(inverseMatrix)
        inverseMatrix
}


## To test run
## x <- rbind(c(2, -3), c(-3,2))    	## make a matrix
## m <- makeCacheMatrix(x)				## create the special matrix
## m$get()								## get the matrix (should be same as x)
## cacheSolve(m)						## get the inverse
## cacheSolve(m)						## get the cached inverse