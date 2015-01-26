## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function does the following
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y)
	{
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i
	list(set=set, get=get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function
## cacheSolve checks that the matrix is not null
## and it is same as the original matrix used in makeCacheMatrix
## If both conditions are valid then it checks if the inverse is cached and it is not null
## Returns the inverse from cache if available, or else saves the inverse in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i) && !(x == x$get()))
        {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)
        i
}
