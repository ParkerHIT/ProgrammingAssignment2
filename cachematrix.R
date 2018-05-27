## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function "makeCacheMatrix" creates a special "matrix" object
## which can cache its inverse. This includes setting the matrix,
## getting the matrix, setting the inverse and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x #get the value of the Matrix
    setinv <- function(inverseMatrix) invMatrix <<- inverseMatrix #set the value of the invertible matrix
    getinv <- function() invMatrix #get the value of the invertible matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

## The function "cacheSolve" computes the inverse of the special "matrix"
## returned by the "makeCacheMatrix" function above. If the inverse has already
## been calculated and the matrix has not changed, then the below
## function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinv()
    if(!is.null(invMatrix)) {  #if inverse matrix is not NULL
        message("getting cached data")
        return(invMatrix) #return the invertible matrix
    }
    data <- x$get() #get the original Matrix Data
    invMatrix <- solve(data) #use solve function to inverse the matrix
    x$setinv(invMatrix) #set the invertible matrix
    invMatrix #return the invertible matrix
}