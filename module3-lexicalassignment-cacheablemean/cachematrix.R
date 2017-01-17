## Functions to allow accessing a matrix and getting inverse where inverse can be cached

## function to create a type of matrix that allows for having its inverse cached
makeCacheMatrix <- function(x = matrix()) {
    #variable for inverse
    i <- NULL
    
    #set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #get the matrix
    get <- function() x
    
    #set the inverse
    setinv <- function(inv) i <<- inv
    
    #get the inverse
    getinv <- function() i
    
    #this list of functions is used to access the matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cache the inverse of the matrix 
## funtion used to access inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # first check for a cached inverse
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #no cached inverse so get the matrix object
    data <- x$get()
    
    #and use it to compute the inverse
    i <- solve(data, ...)
    
    #then store the inverse in the cache variable
    x$setinv(i)
    
    #and return the inverse
    i
}
