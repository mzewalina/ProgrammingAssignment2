## Caching inverse of matrix and solving for its inverse
## makeCacheMatrix is a function that caches a matrix like 



makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
       get <- function() x
       setInverse <- function(inverse) inv <<- inverse
       getInverse <- function() inv
       list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function (cacheSolve) computes the inverse of the  matrix created above 
## If the inverse has already been calculated 

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       
       inv <- x$getInverse()
       if (!is.null(inv)) {
              message("Getting cached data")
              return(inv)
       }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv
}
