## The goal of the following functions is to help save computational time for calculating the 
## inverse of a matrix.  By caching the calculated inverse matrix, you can easily call
## the cached matrix instead of re-computing each time you need it (as long as the
## matrix does not change)

## The goal of the makeCacheMatrix is to create functions that set values for an object (a matrix)  
    ## and its inverse, as well allow you to call those values.
    ## The matrix must be square to return its inverse....
## The set function sets the value of a square matrix with data (y) and nrow/ncol (z)
    ## These values are set beyond the current environment by using <<- instead of <-
    ## We need to make sure to set the inverse variable to NULL so that it can be properly 
    ## recalled or created in the cachSolve function.  
## The get function simply returns whatever matrix is created/set in the "set" function
## The setinverse function creates the "inverse" variable by solving the square matrix (x)
    ## from the "set" function.  This variable also has a scope beyond the current
    ## environment with the <<- set up.
## The getinverse function returns the output from setinverse
## Finally, it is important to store a list of the 4 functions so that when an object is
    ## assigned to the makeCacheMatrix function, the object will have all 4 functions
    ## (i.e., you can call the indivdiual functions associated with the object)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        #Set value of the (square) matrix
        set <- function(y, z) {
                x <<- matrix (y, nrow = z, ncol = z)
                inverse <<- NULL ## necessary to differentiate between a cached inverse
                                 ## & created inverse (i.e., call a cached value)
        }
        get <- function()x  # Assigns the output of the matrix created with the "set" function
        setinverse <- function(solve) inverse <<- solve(x)  # Set value of the inverse matrix
        getinverse <- function() inverse   # Assigns the value of the inverse created with the 
                                           # setinverse function
        #Store a list of the four functions outlined above as the call for the makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function either calls the cached inverse matrix defined by the makeCacheMatrix 
    ## function or calculates it for a new/changed matrix.
## The first step of this function retrieves the cached inverse matrix if it has already been 
    ## created (and the matrix did not change). 
    ## This is accomplished by checking to see if the inverse value is NOT Null.  If there is 
    ## already a value stored for inverse, it will display the message "Getting cached data" 
    ## and return the cached, calculated inverse stored by the "getinverse" function.
## The second step is the "else" part of the function where if the inverse object's value is
    ## equal to Null, it solves the inverse of square matrix x.  It also stores the inverse  
    ## matrix in the setinverse function, so that it can be retrieved as a cached value 
    ## in the future.
## With either step, the inverse matrix is returned as the function output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()  ## calling the inverse of object x stored in the "getinverse" function
        if(!is.null(inverse)) {    ## if there is a matrix stored for inverse, return cached matrix
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

