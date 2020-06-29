## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the input x as a matrix
## m is set to null
## change every mean to Inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse)m <<- inverse
	getInverse <- function()m
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## calculates the inverse of given matrix. however, it checks if the inverse has already been calculated.
## if yes, it retrives it from cache.
## if not it goes about computing the inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setInverse(m)
	m
}
