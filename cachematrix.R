# makeCacheMatrix and cacheSolve functions for
# R Programming - Programming Assignment 2: Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
        # @x: an invertible matrix
        # makeCacheMatrix returns: a list containing functions to:
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse of the matrix
        # 4. get the inverse of the matrix
        
        matrix_inverse = NULL
        set <- function(z) {
                x <<- z
                #`<<-` assigns a value to an object in an environment 
                # different from the current environment. 
                matrix_inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) matrix_inverse <<- inverse
        get_inverse <- function() matrix_inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
        # @x: in the way makeCacheMatrix and cacheSolve work today,
        # x is the output of makeCacheMatrix()
        # cacheSolve returns: inverse of the original matrix input to
        # makeCacheMatrix()
        matrix_inverse <- x$get_inverse()
        if(!is.null(matrix_inverse)) {
        # this if statement checks if the matrix_inverse has already been
        # calculated.
                message("getting cached data")
                return(matrix_inverse)
        }
        # if the matrix_inverse has not yet been calculated, cacheSolve
        # calculates the matrix_inverse and caches it.
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$set_inverse(matrix_inverse)
        # cache the inverse
        matrix_inverse
}
