## makeCacheMatrix is used to create a list which allows you to:
## 1. set a matrix, 
## 2. retrieve that matrix, 
## 3. set its inverse
## 4. retrieve that inverse
## cacheSolve will first check if a matrix inverse has already been computed, 
##  - If so, it will return that inverse without re-computing it
##  - If not, it will compute the inverse, and then cache and return that result

## makeCacheMatrix creates a list object, x(set, get, setinv, getinv)
##  1) x$set(M) - Sets and caches an input matrix M, 
##              - Simultaneously sets the inverse matrix to NULL
##  2) x$get() - Returns a cached matrix M
##  3) x$setinv(N) - Sets and caches a matrix N as the inverse of M
##         ***NOTE: Does not verify that N is the inverse of M
##         ***USE cacheSolve(x) to properly set the inverse matrix N for M
##  4) x$getinv() - Returns the cached inverse matrix N

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a CacheMatrix, x, as an input and then determines:
##   1) Does x already have a cached inverse?  
##       If so, it returns the cached inverse.
##       If not, it computes the inverse, and then caches and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
