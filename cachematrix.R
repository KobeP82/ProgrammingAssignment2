### This R script contains two functions: 
        ### makeCacheMatrix() which creates an 'object' of type 'list' that stores two things:
                ### (1) the original matrix 
                ### (2) the cached inverse matrix, which is initially set to 'NULL'
        ### cacheSolve() which accesses the object created with makeCacheMatrix() and retrieves the inverse
        ### or if the inverse does not yet exist, calculates the inverse matrix
                
## makeCacheMatrix
        ### The function contains four functions:
                ### two to read (or 'get') the value of the matrix and inverse matrix
                ### two functions to change ('set') the value of the matrix and inverse matrix
        ### These functions are defined but not run when makeCacheMatrix is called 
        ### Instead they will be used by CacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # resets the inverse matrix 'inv' to NULL every time makeCacheMatrix is called
        set <- function(y) { # takes and saves the input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # returns the original matrix
        setinverse <- function(inverse) inv <<- inverse # stores the inverse matrix using superassignment
                # setinverse is called by cacheSolve() when determining the inverse the first time for a new matrix
        getinverse <- function() inv # returns the inverse matrix
                # getinverse is used by cacheSolve() to call the inverse matrix after having calculated it once
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                   
}


## cacheSolve
        ### cacheSolve will try to retrieve the inverse matrix from the object created by makeCacheMatrix
        ### if there is no inverse matrix stored (NULL is returned) the function will
                ### (1) calculate and return the inverse
                ### (2) store the inverse in the object for future use

cacheSolve <- function(x, ...) {        # x input here is an object created by makeCacheMatrix
        inv <- x$getinverse()           # cacheSolve will try to retrieve the inverse from the object
        if(!is.null(inv)) {             # if an inverse matrix is found in the object the function will
                message("getting cached data")  # print the message 'getting cached data'
                return(inv)                     # return the inverse matrix
        }
        data <- x$get()                 # if no inverse exists yet in the object
        inv <- solve(data, ...)         # the inverse is calculated from the original matrix stored in the object
        x$setinverse(inv)               # the inverse is stored in the object for future use
        inv                             # return the inverse matrix
}
