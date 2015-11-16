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
        
        # Initially initialize inv to Null
        inv = NULL

        #  Set the matrix
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        
        # Get the matrix
        get = function() x

        # Set the inverse
        setinv = function(inverse) inv <<- inverse 
        ## Get the inverse
        getinv = function() inv

        # This list of functions is returned
        #              1. set the matrix
        #              2. get the matrix
        #              3. set the inverse
        #              4. get the inverse
        # This list is used as the input to cacheSolve()
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
        inv = x$getinv()
        
        # If the inverse has already been calculated, return the cached copy
        # of the inverse
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, calculate the inverse 
        # if mat.data is a square invertible matrix, then solve(mat.data) returns its inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # Set the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        # Return a matrix that is the inverse of the original matrix input to makeCacheMatrix()
        return(inv)
}
