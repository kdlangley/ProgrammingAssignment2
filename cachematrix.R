## makeCacheMatrix creates matrix to set value of matrix, get value of matrix,
## set value of inverse, get value of inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks to see if inverse has already been calculated. If so it gets
## the inverse from the cache and skips computation, otherwise it calculates
## the inverse of the matrix and sets value of inverse in the cache with setinverse
## function. Assumes matrix is always invertible.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
