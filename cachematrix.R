## makeCacheMatrix :
## This function returns a set of functions that do the following:
## 1. Set the matrix value
## 2. Get the matrix value
## 3. Set the inverse value
## 4. Get the inverse value


makeCacheMatrix <- function(x = matrix()) {
	 inverseMatrix <- NULL ## inverseMatrix stores the cached inverse
	 
	    ## to set matrix
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
        
        ##
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## CacheSolve
## This function yields the inverse matrix. If this has been done previously, the cached inverse is then returned.

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
## x <- rbind(c(2, -3), c(-3,2))    ## make a matrix
## m <- makeCacheMatrix(x)			## create the special matrix
## m$get()							## get the matrix (should be same as x)
## cacheSolve(m)					## get the inverse
## cacheSolve(m)					## get the cached inverse