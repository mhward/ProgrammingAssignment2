## The following functions help save computational time for calculating the inverse of a matrix.
## By caching an object, you can easily call the cached value instead of re-computing each time.
## In order to call values beyond their function environment, is important to use <<- instead of <-

## The goal of the makeCacheMatrix function is to create functions that set values for an object  
    ## (a matrix) and its inverse, as well allow you to call the output of those objects.
## The set function sets the value of a square matrix with data (y) and nrow/ncol (z)
## The setinverse function creates the "inverse" variable by solving the square matrix (x)
## The "get" functions simply return whatever values are created/set in the "set" functions
## Finally, it stores a list of the 4 functions so that when an object is assigned to the
    ## makeCacheMatrix function, you can call all 4 functions for the object

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y, z) {
                x <<- matrix (y, nrow = z, ncol = z)
                inverse <<- NULL ## necessary to differentiate between a cached inverse
                                 ## & created inverse (i.e., call a cached value)
        }
        get <- function()x  
        setinverse <- function(solve) inverse <<- solve(x) 
        getinverse <- function() inverse   
        #Store a list of 4 functions outlined above as call for the makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function either calls the cached inverse matrix defined by the makeCacheMatrix 
    ## function or calculates it for a new/changed matrix.
## The first step of this function retrieves the cached inverse matrix if it has already been created
    ## (by checking if the inverse value is NOT Null).  Returns value stored in "getinverse". 
## The second step is the "else" call of the function where if the inverse object's value is Null,
    ## it solves the inverse of square matrix x.  
## With either step, the inverse matrix is returned as the function output.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()  ## calling the inverse of object x stored in the "getinverse" function
        if(!is.null(inverse)) {    
                message("Getting cached data")
                return(inverse)
        }
        ## Else, if inverse=NULL, create the inverse for object x by solving the matrix
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)  ## Set the inverse matrix in setinverse so that it can be 
                               ## recalled as a cached value next time
        inverse
}


##Test the above functions
##  m <- matrix(rnorm(100), 10)
##  a <- makeCacheMatrix(m)
##  a$get()
##  a$getinverse()  #should be NULL"
##  cacheSolve(a)
##  a$getinverse()  #should return inverse value
##  cacheSolve(a)  #should say "Getting cached data"

