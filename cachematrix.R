## This twi functions has the objective to create a specil matrix and compute the matrix inverse 
## The function makeCacheMatrix creates an objecy the is able to store a cache of inverse of matrix
## the function CacheSolve compute the inverse if it was not calculate yet and store on cache, but if the inverse Matrix is already on cache
##  cacheSolve just return the cached value avoid extra computation

## makeCacheMatrix create a special Matrix object and cache the inverse of the original matrix

makeCacheMatrix <- function(m = matrix()) {
        
            inverse <- NULL
        
            set <- function(y) {
                    m  <<- y
                    inverse <<- NULL
            }
            
            get <- function() m
            
            setinverse <- function(Matrix_Inverse) inverse <<- Matrix_Inverse
            
            getinverse <- function() inverse
            
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## This function calculates the inverse o a matrix, But before doing the computation it's check if the inverse was previously calculedte and stored in the cache.
## if the inverse o the matrix is cached the function return the cache, else it compute the inverse store in cache and retur the inverse.
## In this function implementation is assumed that all matrix can be inverted it's mens that det() is different of 0(zero)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        inverse <- solve( x$get() )
        x$setinverse(inverse)
        inverse
}







