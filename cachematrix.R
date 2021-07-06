## Put comments here that give an overall description of what your
## functions do 

## Write a short comment describing this function
## It can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function( y ) { # set matrix
            m <<- y
            i <<- NULL
        }
        get <- function() {m} # get matrix
        setinverse <- function(inverse) { i <<- inverse } # set inverse matrix
        getinverse <- function() {i} # get inverse matrix
        list(set = set, get = get, # return list
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if( !is.null(m) ) {. #If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.
                message("getting cached data")
                return(m)
        }
        data <- x$get() # get the matrix
         m <- solve(data) %*% data # 
         x$setInverse(m) # set inverse
         m # return result
}
