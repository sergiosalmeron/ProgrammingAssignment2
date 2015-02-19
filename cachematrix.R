## These functions create an object that alows to store a matrix and 
## its inverse (when calculated). This way the inverse has to be calculated
## just once, regardless the number of times its inverse is accesed.
## example:
## x<-makeCacheMatrix(matrix(rnorm(9),3,3))
## cacheSolve(x)

## This function creates the cached matrix
## x is the input matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	#the variables are initialized ('x' contains the original matrix and 'i' will contain the inverse when calculated)
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get,
	   setinv = setinv,
	   getinv = getinv)
}


## This function calculates the inverse of a matrix or loads it if it has already been calculated
## x is the input cacheMatrix

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	#if the inverse matrix has been calculated (so it has been stored in 'i'), the value of i is returned
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	#if the inverse matrix has NOT been calculated, it is calculated and the result is stored in 'i'
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
