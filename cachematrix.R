## Put comments here that give an overall description of what your
## functions do

##
# This represents a cacheMatrix class. 
# 
#

makeCacheMatrix <- function(x = matrix()) {
    inverted <- matrix(nrow=dim(x)[1], ncol=dim(x)[2])
    x_copy <- x
    dirty <- TRUE
    set <- function(y) {
        if( !identical(y,x_copy) ) {
            x_copy <<- y
            dirty <<- TRUE
        }
    }
    get <- function() {
        x_copy
    }
    setinv <- function( x=inverted ) {
        dirty <<- FALSE
        inverted <<- x
        inverted
    }
    getinv <- function() {
        inverted
    }
    modified <- function() dirty
   
    tmp<-list(get=get, set=set , getinv=getinv, setinv=setinv,modified=modified)
    class(tmp) = "cacheMatrix"
    tmp
}

#
# Allows us to print out a cacheMatrix as if it was a regular matrix

print.cacheMatrix = function(obj) {
    print(obj$get())
}

#
# [] operator for accessing the cacheMatrix.
#
`[.cacheMatrix` = function(obj,...) {
    obj$get()[...]
}

#
# []<- operator for allow us to modify the contents
# of the cacheMatrix
`[<-.cacheMatrix` = function(obj,i,j,..., value) {
    args <- list(...)
    printf("args was %s\n", class(args))
    printf("i was %s",class(i))
    tmp <- obj$get()
    if( ! missing(j) ) { 
        printf("j was %s\n",class(j))
        tmp[i] <- value
        obj$set(tmp)
    } else {
        printf("j was missing\n");
        tmp[i,j] <- value
        obj$set(tmp)
    }
    obj
}

## cacheSolve(x,...)
# 
# First determines if the matrix has been "dirtied" by any of
# the last modifications, and if so, then calculates the inverted
# matrix , sets it and then returns it.
#
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    if( x$modified() ) {
        retval <- x$setinv( solve(x$get()) )
    } else {
        printf("Nonmodified\n")
        retval <- x$getinv()
    }
    retval
}


