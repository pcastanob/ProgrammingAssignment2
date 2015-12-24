## Matrix inversion is usually a costly computation. 
## The functions below will cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## set the matrix and put the inverse back to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the matrix
    get <- function() x
    
    ## cache the inversed matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## get the inversed matrix
    getinverse <- function() i
    
    ## create the list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## If the inverse has already been calculated (and the matrix has not 
    ## changed), then return the inverse from the cache.
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## otherwise calculate the inverse, add it to the cache and return the inverse.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Test functions with an example.
message("EXAMPLE:")
mat <- makeCacheMatrix()
mat$set(matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3))
message("Matrix:")
print(mat$get())
message("Inverse:")
print(cacheSolve(mat))
message("Inverse:")
print(cacheSolve(mat))
