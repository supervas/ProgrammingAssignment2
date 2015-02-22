## This functions show the advantajes of the scope rules of the R
## language and how they can be manipulated to preserve state inside of an
## R object. 
## Through the following two functions is calculated inverse of a given matrix and, 
## if you have been calculated previously, just returned from the cache which is 
## already calculated. This cache is accessible thanks to the Working Environment.

## Function
# Name:         makeCacheMatrix
# Description:  Defines a set of functions for storing and returning Matrix in cache.
# Use:
#       Matrix_to_be_cache <- makeCacheMatrix(matrix(x:x,x))
    
makeCacheMatrix <- function(x = matrix()) 
{
    # Global variable to Store cached Matrix.
    vCacheMatrix <- NULL
    
    # Function to set Matrix and initialize cache Matrix in Working Environment (<<-).
    set <- function (pMatrix)
    {
        x <<- pMatrix
        vCacheMatrix <<- NULL
    }
    
    # Get initial Matrix.
    get <- function() x
    
    # Apply function to initial matrix (eg. solve, t, etc.) and store the result in Cache.
    setCalculatedMatrix <- function(pCalculatedMatrix) vCacheMatrix <<- pCalculatedMatrix
    
    # Returns matrix with applied function.
    getCalculatedMatrix <- function() vCacheMatrix
    
    # Return set of methods available in function.
    list (set = set, 
          get = get, 
          setCalculatedMatrix = setCalculatedMatrix,
          getCalculatedMatrix = getCalculatedMatrix)   
}


## Function
# Name:         cacheSolve.
# Description:  Calculate de inverse of Matrix passed by parameter.
#               If Matrix passed by parameter not exists in Cache, calculate inverse of Matrix
#               and save in Cache.
#               If Matrix passed by parameter exists in Cache, return the Matrix from cache.
# Use:
#       cacheSolve(previus_Matrix_Cached)

cacheSolve <- function(x, ...) 
{
    # Capture Inverse of Matrix from Cache.
    vInverseMatrix <- x$getCalculatedMatrix()
    
    # If Inverse Matrix exists in Cache, return value.
    if (!is.null(vInverseMatrix))
    {
        message("getting Cached Data.")
        return (vInverseMatrix)
    }
    
    # Matrix not exists in Cache. Initialize Inverse Matrix in Cache.
    x$setCalculatedMatrix(solve(x$get()))
    
    # Return Inverse Matrix.
    return (x$getCalculatedMatrix())
}
