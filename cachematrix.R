# makeCacheMatrix creates a special "matrix", 
# it is a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix value
    inv <- NULL
    
    # setting value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # getting value of the matrix
    get <- function() x
    
    # setting value of the inverse
    set_inverse <- function(inv_input) inv <<- inv_input
    # get the value of the inverse
    get_inverse <- function() inv
    
    # returning a list of all the above functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

## cacheSolve calculates the inverse of the 
## "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## before getting the inverse from the cache and skipping the 
## computation. If not, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache using 
## the setinv function.

cacheSolve <- function(x, ...) {
    # check if the inverse is already cached,
    # if so, we get the inverse from the cache directly
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else, we first get the matrix
    data <- x$get()
    # and calculate the inverse
    inv <- solve(data, ...)
    # next, cache the inverse of the matrix
    x$set_inverse(inv)
    # and finally, return the result
    inv
}
