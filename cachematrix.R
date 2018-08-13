## Put comments here that give an overall description of what your
## functions do
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
		## Inverse property initialization
		m <- NULL
	## Method to set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	
	## Method to set the matrix
        get <- function(){
		x
		}
		
	## Method to set the inverse of the matrix
        setInverse <- function(inverse){
		m <<- inverse
		}
		
	## Method to get the inverse of the matrix
        getInverse <- function(){
		m
		}
	 ## Returns a list of methods
        list(set = set, get = get,
            setInverse = setInverse,
	    getInverse = getInverse)
}

## Compute the inverse of matrix returned by "makeCacheMatrix" above. If the inverse is already calculated, then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		
	## Return inverse of 'x' matrix 
        m <- x$getInverse()
	
	 ## If the cached matrix inverse is set, then return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	
	 ## Get the matrix from x 
        data <- x$get()
		
	## Inverse calculation using matrix multiplication
	m <- solve(data) %*% data
	
	## Set the inverse of matrix x 
        x$setInverse(m)
        
	## Return a matrix that is the inverse of 'x'
	m
}
