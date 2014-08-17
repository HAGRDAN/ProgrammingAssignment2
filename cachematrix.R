## Dan Hagrman

## R Programming Assignment 2

## makeCacheMatrix returns a wrapper around the input matrix.
## The wrapper holds the 
##   input matrix (A), 
##   the inverse matrix (B), 
##   and a list of functions, namely:
##
##   1. set   - sets the original matrix A
##   2. get   - gets the original matrix A
##   3. set_B - sets the inverse matrix B
##   4. get_B - gets the inverse matrix B (can be NULL)


## cacheSolve expects a wrapper returned by makeCacheMatrix.
## It checks the value of B; if not null, this means it has
## already been calculated, and it is then returned.
## If B is NULL, it gets A, calculates B, saves it into
## the wrapper, then returns B.

## To use these classes:
## 1. Send each matrix into makeCacheMatrix; save the wrapper.
## 2. When a matrix inverse is desired, send the wrapper
##    corresponding to the matrix to cacheSolve.
## 3. The inverted matrix is returned.



## makeCacheMatrix returns a wrapper that holds the 
## original and inverted matrices.

## A is the input matrix for the returned wrapper Awrap.
## B is the inverted matrix (the "answer") 
##    which is stored in Awrap$B the first time cacheSolve is called.

makeCacheMatrix <- function(A = matrix()) {
    ## Return a wrapper around the matrix provided.

    # initially set the inverted matrix to solve for to NULL.
    B <- NULL
    
    # Define the get & set functions in this wrapper
    # for cacheSolve to use.
    set <- function(y) {
        A <<- y      # Assign the matrix to the wrapper's holder
        B <<- NULL   # Assign an initially NULL answer 
    }

    get <- function() A
    set_B <- function(solve) B <<- solve
    get_B <- function() B
    
    #return:
    list(set = set, get = get,
         set_B = set_B,
         get_B = get_B
    )
}

## cacheSolve expects a wrapper returned by makeCacheMatrix.
## It checks the value of B; if not null, this means it has
## already been calculated, and it is then returned.
## If B is NULL, it gets A, calculates B, saves it into
## the wrapper, then returns B.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    B <-x$get_B()

    if(!is.null(B)) {
        message("returning cached matrix")
        return(B)
    }
    
    # If we're here, B is not cached.  
    # Get the original matrix that was sent to makeCacheMatrix
    # and stored in this wrapper, then calculate the
    # inverse and save it to B.
    # the makeSolve
    data <- x$get()
    B <- solve(data, ...)
    x$set_B(B)
    
    # return the inverted matrix we just solved and stored:
    B    
}


##############################################
## END OF ASSIGNMENT
##############################################

## These are sample inputs & the original functions.
## Please ignore.

# v1 <- c(1,2,3,4,5)
# v1
# [1] 1 2 3 4 5
# cv1 <- makeVector(v1)
# cachemean(cv1)
# [1] 3
# cachemean(cv1)
# getting cached data!!!
# [1] 3

# vec <- c(1, 2, 3)
# vec2 <- makeVector(vec)  ## vector to a list!
# cachemean(vec2)

## These are the examples provided
### makeVector is provided.  It 

## Build a wrapper around the vector provided.
## m is the mean
makeVectorWrapper <- function(x = numeric()) {
    # initially set the mean to NULL.
    theMean <- NULL
    
    # DEFINE functions in the wrapper.
    set <- function(y) {
        x <<- y            # ASSIGN the vector to the wrapper
        theMean <<- NULL   # ASSIGN the mean to the wrapper
    }
    get <- function() x
    set_Mean <- function(mean) theMean <<- mean
    get_Mean <- function() theMean
    
    #return 
    list(set = set, get = get,
         set_Mean = set_Mean,
         get_Mean = get_Mean
         )
}
### cachemean doesn't take a vector!
##  it takes a LIST of FUNCTIONS created by makeVector!!
cachemean <- function(xWrap, ...) {
    
    # First check and see if theMean is known
    # by "calling" the function pointer.
    aMean <- xWrap$get_Mean()
    if(!is.null(aMean)) {
        message("getting cached data")
        return(aMean)
    }
    message("calculating mean")
    # calculate the mean and save it into the wrapper.
    data <- xWrap$get()
    aMean <- mean(data, ...)
    xWrap$set_Mean(aMean)
    
    #return the calculated mean
    aMean
}

# setwd("C:\\save\\Dropbox\\Coursera\\R\\ProgrammingAssignment2")
# source ("C:\\save\\Dropbox\\Coursera\\R\\ProgrammingAssignment2\\cachematrix.R")

