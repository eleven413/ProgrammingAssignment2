## The first function below returns a list containing 4 functions. 
## If a matrix is passed to the function makeCacheMatrix, it returns a list of 4
## functions that can then be further passed to the next function (cacheSolve) to
## compute the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The below function takes a matrix, computes its inverse and returns the result. 
## Prior to computing the inverse, it checks to see if that inverse has already been 
## calculated; if yes, then it simply retrieves the cached (or saved) result instead of 
## re-computing. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
