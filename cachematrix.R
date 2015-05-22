## makeCacheMatrix function creates a special 'Matrix' that can cache its inverse.
## This contains the methods to set, get the values of elements of the Matrix and set and get inverse
makeCacheMatrix <- function ( x = matrix()) {
	## initialize inverse to NULL
	inv <- NULL

	## set function 
	set <- function(y) {
		x   <<- y
		inv <<- NULL
	}
	
	## get function 
	get      <- function(){
	 	  x
	}
	
	## set inverse function 
	setsolve <- function    (inv1) { 
		inv <<- inv1 
	}

	## get inverse function 
	getsolve <- function () {
		inv
	}
	## create list of functions
	list( set = set, get = get, setsolve = setsolve , getsolve = getsolve )
}

## This function checks if the inverse exists in Cache. 
##If it doesn't exist in cache it computes the inverse and sets the cache
cacheSolve <- function (x) {
	## get inverse from cache
	inv <- x$getsolve ()
	## check if inverse exists in cache. If it does retun inverse
	if (!is.null(dim(inv)))
 	{
		message("getting cached data")
		return (inv) 
	}
	## if inverse doesnt exist in cache, create inverse using solve function
	data <- x$get()
	inv <- solve(data)
	## set the inverse using setsolve method
      x$setsolve(inv)
	inv 
}