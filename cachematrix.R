## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is time-consuming computations. 
## Rather than repeatedly computing, we can cache the inverse of a matrix. 
## So that when we need it again, it can be looked up in the cache rather than recomputed
## This is achieved by two R functions: "makeCacheMatrix" and "cacheSolve"
## Write a short comment describing this function
## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
+ set <- function(y) {
+ x <<- y
+ inv <<- NULL
+ }
+ get <- function() x
+ setinv <- function(inverse) inv <<- inverse
+ getinv <- function() inv
+ list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+inv <- x$getinv()
+ if(!is.NULL(inv)) {
+ message("getting cached result")
+ return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data, ...)
+ x$setinv(inv)
+ inv
}
