## Put comments here that give an overall description of what your
## functions do

## funtion wich creates a list of functions and caches the matrix

makeCacheMatrix <- function(x = matrix()) {
    # we make a dummy inverse value,save the value for later
    thisinverse <<- NULL
    #function that sets a different value to the matrix
    set <- function(y){
        #if we set the value of the matrix again, the inverse should be NULL
        if(is.matrix(y)){
            #save the value for later!!!
            x <<- y
            thisinverse <<- NULL
        }
        else{
            message("Not a matrix")
        }
    }
    #function that gives the value of "x", returns the matrix
    get <- function() x
    #function that sets the inverse matrix
    setinverse <- function(inverse){
        if(is.matrix(inverse)){
            #save the value of the inverse so anybody can check it ;)
            thisinverse <<- inverse
        }
        else{
            message("Not a matrix")
        }
    }
    #funtion that returns the value of the inverse, no matter what
    getinverse <- function() thisinverse
    #at last return a list of all the functions
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Function that solves the inverse of matrix if not cached
## returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #get the value of the inverse, no matter what
    myinverse <- x$getinverse()
    #if it is not NULL, we return what we got
    if(!is.null(myinverse)){
        message("getting what was cached :)")
        return(myinverse)
    }
    #if not, get the matrix and calculate the inverse
    thematrix <- x$get()
    theinverse <- solve(thematrix,...) 
    #cache the value
    x$setinverse(theinverse)
    #return the inverse of the matrix
    theinverse
}
