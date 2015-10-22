#Programming Assignment 2, Week 3
#This code includes one function that creates a "matrix" list object that
#can cache the inverse of the matrix once it is calculated.
#A second function calculates the inverse of the matrix created by the first 
#function, or returns the cached value if the matrix has not changed since
#the inverse was last calculated.

#the makeCacheMatrix function creates a matrix list object that contains
#four internal functions:
#  set: set the value of the matrix
#  get: get the value of the matrix
#  setinverse: set the value of the inverse of the matrix
#  getinverse: get the value of the inverse of the matrix

#x is the persistent variable within the makeCacheMatrix function
#that stores the original matrix
#inv is the persistent variable within the makeCacheMatrix function 
#that stores our cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solvematrix) inv <<- solvematrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The cacheSolve function returns the inverse of the matrix from the object 
#created by the function above. If the inverse has already been calculated, 
#it returns the cached value. Otherwise, it uses Solve to determine the
#inverse of the matrix and caches it in the object.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
