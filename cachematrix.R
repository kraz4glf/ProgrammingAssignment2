## These are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
## It returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        
        # Initially initialize inverse_matrix to Null
        inverse_matrix = NULL

        #  Set the matrix
        set = function(y) { 
                x <<- y
                inverse_matrix <<- NULL
        }
        
        # Get the matrix
        get = function() x

        # Set the inverse
        setinv = function(inverse) inverse_matrix <<- inverse 
        ## Get the inverse
        getinv = function() inverse_matrix

        # This list of functions is returned and used as the input to cacheSolve()
        #              1. set the matrix
        #              2. get the matrix
        #              3. set the inverse
        #              4. get the inverse
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        
        # Get the value of the inverse in the cache via the getinv function.
        # This is Null if the inverse has not been previously calculated
        inverse_matrix = x$getinv()
        
        # If the inverse has already been calculated, return the cached copy
        # of the inverse
        if (!is.null(inverse_matrix)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse_matrix)
        }
        
        # Otherwise, calculate the inverse 
        # if data is a square invertible matrix, then solve(data) returns its inverse
        data = x$get()
        inverse_matrix = solve(data, ...)
        
        # Set the value of the inverse in the cache via the setinv function.
        x$setinv(inverse_matrix)
        
        # Return a matrix that is the inverse of the original matrix input to makeCacheMatrix()
        return(inverse_matrix)
}
