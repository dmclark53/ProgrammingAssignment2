## These two functions concern the inverse of a matrix. If the inverse does not
## exist then it is created and cached, otherwise the cached inverted matrix is
## used.

## This function caches the inverted matrix. It defines four functions to:
## 1) Set input matrix to NULL
## 2) Get input matrix
## 3) Set inverted matrix
## 4) Get inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                y <<- x 
                m <<- NULL         }
        
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}

# This function computes the inverse of the matrix, if it doesn't exist in cache
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                
                message("Getting cached inverted matrix.")
                return(m)
                
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m

}
