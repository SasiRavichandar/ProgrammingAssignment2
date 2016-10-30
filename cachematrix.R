## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# this function, takes in matrix, sets and retrives matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #initialize 
        
        inv <-   NULL
        #Sets the matrix and assigns globally
        # the matrix which is passed into the function is set assigned globally
        # inv function is initialized
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #  Retrieving the matrix, which is set globally
        
        get = function() x
        #  setting inverse of a matrix (assigning it globally)
        setinverse = function(inverse) inv <<- inverse 
        #getting inverse
        getinverse = function() inv
        list(set=set, get=get, setinv=setinverse, getinv=getinverse)

}


cacheSolve <- function(x, ...) {
        #call get inv function
        
        inv <- x$getinverse()
        
#Check for if inverse exists, if exists, return message and do nothing, else  inverse.
        
        if (!is.null(inv)){ 
                message("getting cached data")
                return(inv)
        }
        
        #GET
        
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        # SET
        
        x$setinverse(inv)
        
        return(inv)
}
