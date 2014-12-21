## As matrix inversion is computationally expensive task, if the matrix values are not going 
## change and there is a need to compute the inverse of the matrix in multiple places with in 
## the program it is better to store the matrix inverse in cache memory so that it can be called 
## whenever needed with out computing the inverse





## This function created and returns the list vector containing functions for the special 
## matrix created (get) and its inverse (getinv), also included in the list are set 
## function, which will define a function to the set matrix, x, to new matrix y and sets 
## inverse (m) to NULL



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The following function takes 'x' matrix as input and tries to get the inverse from the special 
## matrix inverse created from the above function, if it cannot find the inverse 
## (where m is null), the function below computes the inverse and sets the inverse into
## cache memory

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
