## Put comments here that give an overall description of what your
## functions do
## My first function creates a list to store the calculation result
## of the inverse of a matrix from the second function.
## My second function gets the info from the first function, then
## solve the inverse of a matrix, and store the result in the list
## provided by the dirst function at last.

## Write a short comment describing this function
## This function creates a list to store the result from the second function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setivmt <- function(solve) m <<- solve
        getivmt <- function() m
        
        list(set = set, get = get,
             setivmt = setivmt,
             getivmt = getivmt)
}


## Write a short comment describing this function
## This function play a role of calculating the inverse of a matrix
## and reflect the result to the list provided above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getivmt()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setivmt(m)
        m
}
