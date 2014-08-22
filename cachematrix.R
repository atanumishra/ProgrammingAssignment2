## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly
## This is an example of how you would invoke the 2 functions listed below
## > source("C:\\Learning\\R Programming AUG2014\\Programming Assignments\\ProgrammingAssignment2\\cachematrix.R")
##
## Create the test invertible matrix
## > X <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##
## Set up the vector
## > mat <- makeCacheMatrix(X)
##
## Compute the inverse -- in the first run, the matrix inverse has to be calculated
## > cacheSolve(mat)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Execute a second time -- note message about getting cached result
## > cacheSolve(mat)
## getting cached matrix inverse
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
## Execute a third time -- again, pick up the cached result
## > cacheSolve(mat)
## getting cached matrix inverse
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	matrix <- NULL
	set <- function(y) {
			x <<- y
			matrix <<- NULL
	}
	
	get <- function() x
	setInverse <- function(mat) {
		matrix <<- mat
	}
	getInverse <- function() matrix
	
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
			 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
		
	matrix <- x$getInverse()
	if (!is.null(matrix)) {
			message("getting cached matrix inverse")
			return(matrix)
	}
	matrixData <- x$get()
	matrix <- solve(matrixData)
	x$setInverse(matrix)
	matrix
}
