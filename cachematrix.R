## Put comments here that give an overall description of what your
## functions do
## The two functions take a matrix and calculate the inverse matrix.
## The calculated inverse matrix is cached, if the same matrix is input, 
## the functions take the caches matrix as output, instead of recalculating.

## Write a short comment describing this function
#This is similar to the example, except change mean to inv.
#It returns a 2x2 matrix with the 4 functions as elements.
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinv<-function(inv) m<<-inv
	getinv<-function() m
	mat<-matrix(list(set=set, get=get,
	     setinv=setinv, getinv=getinv),2,2)
	names(mat)<-c('set','get','setinv','getinv')
	mat
}


## Write a short comment describing this function
#This function takes the output of the previous function
#if the inversion was calculated and cached, it took the stored inv matrix.
#if not, it recaculates it and cache the inv matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinv(m)
	m
}
