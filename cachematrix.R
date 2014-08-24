## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

# Returned object (in list form) contains following methods:
# set: sets matrix and resets cached inverse
# get: returns matrix
# setSolve: saves solve value
# getSolve: returns cached inverse value
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
        x <<- y
#<<- operator is used to assign a value to x in an environment that is different from the current environment
        inv_x <<- NULL

        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## It takes the object of that type as an argument 'x', checks if the inverse value 
## has already been calculated (and the matrix has not changed) then 
## the cacheSolve will retrieve the inverse from the cache.
## If not, this function calculates the inverse for the matrix saved in the 'x', 
## saves it into 'x' cache using method 'setSolve' and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
        message("cached inverse matrix")
        return(inv_x)
        } 
        else {
#passing matrix to solve
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
        }
}
