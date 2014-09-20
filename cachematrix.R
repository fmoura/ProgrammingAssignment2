## These functions combined gives a method to calc the inverse of a Matrix
## in a cost effective way by caching the inverse result
## functions do

## Creates a special Matrix that caches its inverse value
makeCacheMatrix <- function(x = matrix()) {
       i <- NULL;
       set <- function(y){
           x <<- y
           i <- NULL
       }
       get <- function() x
       getInverse <- function() i
       setInverse <- function(inverse) i <<- inverse
       list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)

}


## Calcs the inverse of matrix. It uses the special matrix internal cached value if it
## is avaible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
            message("Using the cached matrix inverse")
            i
        }
        matrix <- x$get()
        i <- solve(matrix,...)
        x$setInverse(i)
        i
}
