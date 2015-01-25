## Together these functions allow the inverse of a matrix to be stored along with it, so that it only  
## needs to be recalculated if the value of the matrix itself has been reset

## This function creates an object which can store a matrix and it's inverse.
## It returns a list of functions which set or get the values

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	
	set <- function(y){
			x <<- y 
			inv <<- NULL
	}	
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list ( set = set , get = get,
		   setinverse = setinverse, getinverse = getinverse )
}


## This function will check if the object passed to it already has a value set for the inverse.
## If not, it calls a function to calculate the value and sets it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
        		message("getting cached data")
        		return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


