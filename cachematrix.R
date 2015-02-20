## This function, named "makeCacheMatrix", is used to
## create a special "matrix". The object created is actually
## a list which contains several functions as its elements.
## Those functions can be used to do the following:
##
## 1) Set (store) the values of the matrix.
## 2) Get the actual matrix object.
## 3) Set (store) the value of the inverse of the matrix.
## 4) Get the value of the inverse of the matrix.
##
## The purpose of this function is to be ready to store the 
## inverse of the matrix when cacheSolve calculates (IF it's not
## stored already - that's something cacheSolve checks). 
## If the inverse had already been calculated 
## and stored in the past then it be retrieved.


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list (set=set,get=get,setinverse=setinverse,
		getinverse=getinverse)
}



## The following function, named "cacheSolve", checks to see
## if there's a value stored for the inverse of the matrix
## created with makeCacheMatrix. If it's not null (i.e. if
## a value had been stored) then it retrieves said value, all that using
## makeCacheMatrix$getinverse. If not then it calculates the inverse
## of the matrix created with the makeCacheMatrix function, via the 
## help of another function called solve, displays it and also
## stores it, using MakeCacheMatrix$setinverse.


cacheSolve <- function(x, ...) {
	m<- x$getinverse()
	if (!is.null(m)) {
		message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}

