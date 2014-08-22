## Put comments here that give an overall description of what your
## functions do

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
