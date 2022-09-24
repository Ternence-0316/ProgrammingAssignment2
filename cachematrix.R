###### Weeek 3 Assignment: Caching the Inverse of a Matrix ######

### 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
        list(set = set,
             get = get,
             setinverse = setinver,
             getinverse = getinver)
}


### 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setinverse(inver)
        inver
}