## makeCacheMatrix to cache inverse of the given matrix
##
## The function cacheSolve calculates the inverse of the matrix given to it 
## However, it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
## We took advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object to write these functions


## makeCacheMatrix is an R function which is able to cache inverse of the given matrix 
## x represents matrix and inverse represents the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve calculates the inverse of the matrix given to it and returns it.
## However, it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
## We took advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object to write these functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached Inverse")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
