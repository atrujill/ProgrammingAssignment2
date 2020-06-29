## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(ginv) i <<- ginv
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
#by default argument is an empty matrix named x
# it uses <<- operator which can be used to assign a value to an object in an environment that is different from the current environment
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("inverse matrix exist, so we are getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# testing
# sm <- matrix(1:4,2,2)
# mCM <- makeCacheMatrix(sm)
# cacheSolve(mCM)

