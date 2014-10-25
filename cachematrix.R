## 
## Module: cachematrix.R
## 
## Defines:
##   makeCacheMatrix
##   cacheSolve
##
## Note:
## The functions cacheSolve and makeCacheMatrix together with its local functions 
## provide a means to cache the inverse of a matrix. 
## 

##
## Function: makeCacheMatrix
## 
## Returns:    A list of functions for setting and retrieving a matrix and 
##             its inverse.
##             The assigned matrix can be retrieved from the return value  
##             by the 'get' function
##
## Assumption: The parameter 'x' is an invertible matrix
##
makeCacheMatrix <- function(x = matrix()) {
        
        ## local Storage for matrix inverse
        mInv <- NULL

        ##
        ## Assigns the matrix to be inverted
        ## Assumption: y is an invertible matrix
        ## 
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        
        ## 
        ## Gets the assigned matrix to be inverted
        ##
        get <- function() x
        
        ##
        ## Sets the given inverse of matrix in local variable (cache)
        ##
        setInverse <- function(amInv) mInv <<- amInv
        
        ##
        ## Returns the stored inverse of assigned matrix
        ##
        getInverse <- function() mInv
        
        ##
        ## Returns the list of accessor functions
        ##
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 
## Function:  cacheSolve
## Returns:   The inverse of the matrix that is assigned to the parameter 'x'
## Parameter: 'x' is a list of functions returned by makeCacheMatrix function
##
## Note: 
## If the inverse has already been computed then that value is returned.
## Otherwise, inverse of the assigned matrix is computed and stored in cache
## through the makeCacheMatrix object 'x' (by 'x$setInverse')
## 

cacheSolve <- function(x, ...) {

        mInv <- x$getInverse()
        if(!is.null(mInv)) {
                message("Returning cached matrix inverse")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data)
        x$setInverse(mInv)
        mInv
}
