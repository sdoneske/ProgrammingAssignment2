## These functions creates a special object that stores a
## numeric matrix and cache's its inverse 
## Matrix assumes input is an invertible matrix

## makeCacheMatrix creates a special "vector", a list containing
## a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,
		getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix in the special
## vector created with makeCacheMatrix. It checks to see if the
## inverse has already been calculated - if so, it gets the 
## inverted matrix and skips the calculation; if not, it
## calculates the inverse matrix and sets the value of the
## inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
	m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m
}
