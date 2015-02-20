# Date : 2/19/2015
# Author : Sandeep Kolte
# R Programming - Coursera Programming Assignment 2.
#----------------------------------------------------
## This function introduces the <<- operator which can be used to assign a value 
# to an object in an environment that is different from the current environment of the calling function.

makeCacheMatrix <- function(x = matrix()) {
        # Clear
        m <- NULL
        
        # Use the <<- operator to assign value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Get the assigned value
        get <- function() x
        
        # Invert the specified matrix and save it. 
        setinversematrix <- function(solve) m <<- solve
        
        # Return the inverted matrix.
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


#----------------------------------------------------
# cacheSolve
#----------------------------------------------------
# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. 
# 
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.
# Computing the inverse of a square matrix can be done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) returns 
# its inverse.
# 
# Assume that the matrix supplied is always invertible.
#----------------------------------------------------
## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {
        
        # For the given matrix x, check the parent environment to see if there's already an inverse matrix in the cache
        # i.e. saved in free a variable in lexical scope.
        m <- x$getinversematrix()
        
        # If there's a cached version of the inverse matrix, just return that i.e. print it to the screen.
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        
        # Get the value of the matrix
        matrix <- x$get()
        
        # Determine the size of the matrix, this will help raise error when input matrix is singular (cannot be inversed) or not a square.
        size <- dim(matrix)
        
        # If row size and column size is same, this is a square matrix. 
        # However, at this point we still cannot decide whether it can be inversed or not...
        if(size[1] == size[2])
        {
                # This is a very basic check to see if the specified matrix is singular or not.
                # solve(...) is used to invert the matrix. If there is an error when inverting the matrix,
                # probably it is a singular matrix. 
                result = tryCatch({
                        
                        # Get inverse matrix in 'm'
                        m <- solve(matrix, ...)
                        
                        # Save the inverse to cache by setting the environment variable
                        x$setinversematrix(m)
                        
                        # As always, print inverted matrix 'm' to the screen. TODO : This may not be needed.
                        #m
                }, warning = function(w) {
                        message("Warning : Your matrix does not have an inverse.")
                }, error = function(e) {
                        message("Error : Your input matrix appears to be a singular matrix i.e. it does not have an inverse, dgesv returns an error.")
                }, finally = {
                        #cleanup-code
                })
        }
        else
        {
                message("The input matrix is not a square matrix.")
        }
        # Print inverted matrix 'm' to the screen.
        m
}

#----------------------------------------------------
## How to run the code above on R command line:
#----------------------------------------------------
# > x <- makeCacheMatrix(matrix(1:4, 2, 2))
# > x$get()

# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > cacheSolve(x)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > cacheSolve(x)
# getting cached inverse matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > x$set(matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3))
# > x$get()
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0

# > cacheSolve(x)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

# > cacheSolve(x)
# getting cached inverse matrix
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

# To verify the correctness of the inverse matrix above please visit http://www.wikihow.com/Inverse-a-3X3-Matrix
