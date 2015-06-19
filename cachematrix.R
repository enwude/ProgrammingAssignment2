## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        
        # set() recieves an argument and stores it as a matrix in variable x
        # Function set also resets the matrixInv cache to NULL for a new computation
        set <- function(y){
                x <<- y
                matrixInv <<- NULL
        }
        
        # get() returns the stored matrix
        get <- function(){x}
        
        # setInv caches the matrix inverse in matrixInv
        setInv <- function(inverse){
                matrixInv <<- inverse
        }
        
        # function getInv retrieves the cached inverse from matrixInv
        getInv <- function(){matrixInv}
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacheSolve retrieves the inverse from the cache if it has already been computed and stored.

cacheSolve <- function(x, ...) {
        
        ## This function returns a matrix that is the inverse of 'x'
        
        ## matInv calls function getInv in makeCacheMatrix to return the value contained in the cache
        matInv <- x$getInv()
         if(!is.null(matInv)){
                 message("Retrieving matrix inverse from cache...") ## display message if a value is contained in cache
                 return(matInv)
         }
        
        data <- x$get() ## Call function get and store matrix in data
        inverse <- solve(data) ## call function solve  to compute the inverse of matrix
        x$setInv(inverse) ## call setInv to cache the computed inverse
        return(inverse)
                 
}
