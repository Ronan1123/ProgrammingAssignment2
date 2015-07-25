## Put comments here that give an overall description of what your functions do:
## makeCacheMatrix creates a list of 4 items: set, get, setinverse and getinverse
## which are then passed to the CacheSolve function which either calculates the
## inverse value of the input matrix or returns the already existing inverse matrix
## from the cache


## Write a short comment describing this makeCaheMatrix function:
## This function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize a matrix m and set equal to NULL in the local environment
        m <- NULL                         
       
        ## create a subfunction "set" to take in the data, assign to x and set m equal to NULL, all in the global environment
        set <- function(y) {              
        
                x <<- y         ## x (the input matrix) is set to the values of y
                m <<- NULL      ## m is the inverse matrix and is initialized to null, in the global environment         
        }
        
        get <- function() x                             ## a function "get" that returns the input matrix 
        setinverse <- function(solution) m <<- solution ## a function "setinverse" that sets the value of the inverse "m" in the global environment
        getinverse <- function() m                      ## a function called "getinverse" that returns the inverse "m"
        list(set = set, get = get,                      ## an output list from makeCacheMatrix function for later use by cacheSolve function
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function:
## This function either computes the inverse of the matrix x or 
## retrieves the value for that inverse matrix that is stored in the cache
## It takes as input the matrix to be inverted "x" and the "..." argument means 
## it is passed the other 4 functions from makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()                       ## m is locally assigned the value from the input list 
        if(!is.null(m)) {                         ## if m is not empty (i.e. the inverse has already been calculated and exists)
                message("getting cached data")    ## then tell user you're getting the inverse from cache
                return(m)                         ## effectively return the value from the cache
        }
        data <- x$get()                           ## else get the matrix to be inverted from the input list  
        m <- solve(data, ...)                     ## invert it using the solve function and assign the value to m
        x$setinverse(m)                           ## set m in parent environment to new value for m
}

