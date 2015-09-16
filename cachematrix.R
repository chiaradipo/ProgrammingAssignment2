## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates an object used for caching the inverse 
##matrix.
##This output is a list containing four elements:
##the first contains the set function, in order to set the matrix to solve,
##the second returns the matrix x, the third sets the inverse once it's been computed
##and finally the fourth element returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
##I inizialize my inverse to NULL
  inv <- NULL

##then i set the matrix i want to solve and i get it as an element of the output list
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x

##now i set and then get the inverse computed by the cacheSolve function

        setinv <- function(inverse) inv  <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Through this function i'll compute the inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


##first i check if it has already been computed and cached

inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

##then i get the matrix and solve it, the set it in the list and return the inverse

        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}



}
