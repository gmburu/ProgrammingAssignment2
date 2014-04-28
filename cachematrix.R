## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## function that returns the matrix
    get <- function() x
    ## function to store the inverse matrix
    setInv <- function(inverse) inv <<- inverse
    ## vaction that retrieves the matrix inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of a matrix retruned by makeCacheMatrix
## The function returns a cached value of the matrix if the inverse had been computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    ## use the solve function to get the inverse
    inv <- solve(data,...)
    ## call function to store the inverse in cache
    x$setInv(inv)
    inv
}
