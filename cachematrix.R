## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fist function creates the matrix as a list of functions set,get,setsolve and getsolve,
## the firt step is to create the matrix calling the function, for example:
## x <-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## Then you can print the matrix to check if the matrix has been created:
## x$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## The function set an setsolve cached the matrix and it inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Write a short comment describing this function
## This second function calls the previous function and obtains the cache data if exits, if not,
## calculates the inverse of the matrix and returns it.
## To call the function execute:
## cacheSolve(x)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## The second execution (cached) looks like:
## cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
