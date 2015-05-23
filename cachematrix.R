## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "vector" which is a list containing a function to
## 1.    Set the value of the matrix
## 2.    Get the value of the matrix
## 3.    Set the value of the inverse of the matrix
## 4.    Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The function below returns the inverse of the matrix.
## First, checks if inverse has been cached.
## If yes, then gets the cached value and skips comutation.
## If not, then computes the inverse and caches the value using setinverse
## Assumtion -> matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data)
    x$setinverse(invmat)
    invmat
}
