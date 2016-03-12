## creates a cached matrix, which is capable of storing a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set_matrix <- function(new_matrix){
		x <<- new_matrix
		inverse_matrix <<- NULL
	}
	get_matrix <- function () x
	set_inverse <- function(new_inverse_matrix) inverse_matrix <<- new_inverse_matrix
	get_inverse <- function() inverse_matrix
	
	list(	set = set_matrix, 
			get = get_matrix, 
			setinverse = set_inverse, 
			getinverse = get_inverse)
}


## checks if the cacheMatrix has a cached inverse matrix; 
# if yes, returns the inverse matrix from cache
#if not, saves it to cache and returns computed inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)){
        	message("inverse matrix is cached, returning cached matrix")
        	return(inverse_matrix)
        }
        calculated_inverse_matrix <- solve(x$get(), ...)
        x$setinverse(calculated_inverse_matrix)
        calculated_inverse_matrix
}
