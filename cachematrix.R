## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        
        # setMat recieves an argument and stores it as a matrix in variable x
        # Function setMat also resets the matrixInv cache to NULL for a new computation
        setMat <- function(y){
                x <<- y
                matrixInv <<- NULL
        }
        
        # getMat returns the stored matrix
        getMat <- function(){x}
        
        # setInv caches the matrix inverse in matrixInv
        setInv <- function(inverse){
                matrixInv <<- inverse
        }
        
        # function getInv retrieves the cached inverse from matrixInv
        getInv <- function(){matrixInv}
        
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacheSolve retrieves the inverse from the cache if it has already been computed and stored.

cacheSolve <- function(x, ...) {
        
        ## This function returns a matrix that is the inverse of 'x'
        
        ## matInv calls function getInv in makeCacheMatrix to return the value contained in the cache
        matInv <- x$getInv()
         if(!is.null(matInv)){
                 message("Retrieving cached matrix inverse") ## display message if a value is contained in cache
                 return(matInv)
         }
        
        data <- x$getMat() ## Call function getMat and store matrix in data
        inverse <- solve(data) ## call function solve  to compute the inverse of matrix
        x$setInv(inverse) ## call setInv to cache the computed inverse
        return(inverse)
                 
}
