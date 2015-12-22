## this project will create a function for creating a cacheable matrix object
## as well as wrapper function to cache that special object

## creates the cacheable matrix object with getters and setters

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve  ## where the magic happens
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function will check for existance of inverse in cache and return or 
## calculate the inverse and cache for future using stored behavior of cacheable matrix object

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
