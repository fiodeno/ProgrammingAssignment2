## Put comments here that give an overall description of what your functions do

## Create a Matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Creating variable for the inverse matrix
        inverseM <- NULL   
        ## Gives new value to the matrix
        set <- function(y) {
                x <<- y
                ##Resets the matrix
                inverseM <<- NULL
        }
        ## Get the matrix
        get <- function() x 
        ## Set the inverse
        setinverse <- function(inverse) inverseM <<- inverse
        getinverse <- function() inverseM
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inverseM <- x$getinverse()
        if(!is.null(inverseM)) {
                message("getting cached data")
                return(inverseM)
        }
        data <- x$get()
        inverseM <- solve(data, ...)
        x$setinverse(inverseM)
        inverseM
}
