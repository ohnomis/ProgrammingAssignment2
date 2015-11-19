## Caching the Inverse of a Matrix
 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.

## The following two functions are used to cache the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse. It is a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## Set the matrix.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## Get the matrix.
	get <- function() x
	
	## Set the inverse.
	setinverse <- function(inverse) inv <<- inverse
	
	## Get the inverse.
	getinverse <- function() inv
	
	## Return list of the four functions above.
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve calculates the inverse of a matrix, unless the inverse has
## already been calculated, in which case, it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	
	## If the inverse has been calculated, then get it from the cache.
	if(!is.null(inv)) {
		message("Getting the cached inverse of the matrix.")
		return(inv)
	}
	
	## If the inverse has not been calculated, then calculate it.
	matrix <- x$get()
	inv <- solve(matrix)
	
	## Cache the inverse.
	x$setinverse(inv)
	
	## Return the inverse.
	return(inv)
}