## Course:      R Programming @ Coursera.org
##
## File:        cacheMatrix.R
##
## Description: This script introduces two functions, corresponding to the 
##              implementation of Programming Assignment 2. 
##
##              The purpose of Programming Assignment 2 is to provide
##              a solution to caching the inverse of a matrix rather
##              than compute it repeatedly, as this is a costly computation
##              operation.
##              
##              The functions are makeCacheMatrix and cacheSolve and they 
##              are described in the code below.
## 
## Author:      Fabiana S. Santana
##
## Date:        27/07/2014
## 


## Function:    makeCacheMatrix
##
## Description: This function creates a special "matrix" object that can cache
##              its inverse. The special "matrix" is in fact a list,
##              containing the following:
##              1. set - set the matrix
##              2. get - get the matrix
##              3. setinverse - set (calculate) the inverse matrix
##              4. getinverse - get the inversematrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse matrix
        inverse <- NULL
        
        ## Implementation of set
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Implementation of get
        get <- function() x
        
        ## Implementation of setinverse, to obtain the inverse matrix using
        ## the "solve" function
        setinverse <- function(solve) inverse <<- solve
        
        ## Implementation of getinverse
        getinverse <- function() inverse
        
        # Creating the special "matrix" as a list with the properties 
        # described above
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function:    cacheSolve
##
## Description: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already 
##              been calculated and the matrix has not changed, then cacheSolve
##              should retrieve the inverse from the cache.
##
##              Note that the function first checks to see if the inverse 
##              has already been calculated and, if that's the case, it skips 
##              the calculation and uses the cached value. 

cacheSolve <- function(x, ...) {
        ## Obtain the inverse matrix cached
        inverse <- x$getinverse()
        
        ## Verify if a inverse matrix cached exists (in this case, != NULL).
        ## If it exists, obtain and return the cached inverse matrix.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## If a cached inverse matrix doesn't exist, obtain the original matrix
        data <- x$get()
        
        ## Calculates the inverse matrix using the "solve" function
        m <- solve(data, ...)
        
        ## Set the inverse matrix
        x$setinverse(m)

        ## Return a matrix that is the inverse of 'x'
        m
}
