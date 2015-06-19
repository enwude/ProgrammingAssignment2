## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        
        setMat <- function(y){
                x <<- y
                matrixInv <<- NULL
        }
        
        getMat <- function(){x}
        
        setInv <- function(inverse){
                matrixInv <<- inverse
        }
        
        getInv <- function(){matrixInv}
        
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacheSolve retrieves the inverse from the cache if it has already been computed and stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matInv <- x$getInv()
         if(!is.null(matInv)){
                 message("Retrieving cached matrix inverse")
                 return(matInv)
         }
        
        data <- x$getMat()
        inverse <- solve(data)
        x$setInv(inverse)
        return(inverse)
                 
}
