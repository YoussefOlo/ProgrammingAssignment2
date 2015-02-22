## This function is able to cache potentially the invers of a matrix

##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##1.set the value of the Matrix
##2.get the value of the Matrix
##3.set the value of the inverse
##4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the special "Matrix"
##if i's already calculated,it gets the invers from the cache and skips the computation
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
