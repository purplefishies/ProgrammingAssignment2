##
# The following code provides two key functions: makeCacheMatrix() and
# cacheSolve(). makeCacheMatrix() returns an object that is known as the
# cacheMatrix class.  
#
# cacheMatrix class: A class that maintains two matrix representations
# 1. the initial matrix a.k.a "base matrix"
# 2. the inverted version of the "base matrix"
#
# - the inverted matrix will only be updated if the main matrix
#   is "dirtied" : if the underlying matrix has any values changed.

##
# makeCacheMatrix: a constructor for building a "cacheMatrix" (see above)
# class from a regular matrix. This class will have various methods that
# can act on it (see comments below ) as well as "friend" methods that
# can access it using R's  particular FUNCTION.DATATYPE construct.
#
makeCacheMatrix <- function(x = matrix()) {
    inverted <- matrix(nrow=dim(x)[1], ncol=dim(x)[2])
    x_copy <- x
    dirty <- TRUE

    # Get/Setter for cacheMatrix base matrix. This function
    # will only set the "dirty" flag for the cacheMatrix iff
    # the new matrix is different than the previous version.
    set <- function(y) {
        if( !identical(y,x_copy) ) {
            x_copy <<- y
            dirty <<- TRUE
        }
    }
    get <- function() {
        x_copy
    }
    # Get/Setter for cacheMatrix's inverted and actual
    # cached matrix.
    setinv <- function( x=inverted ) {
        dirty <<- FALSE
        inverted <<- x
        inverted
    }
    getinv <- function() {
        inverted
    }

    # Returns whether the dirty flag has been set or not
    modified <- function() dirty
   
    tmp<-list(get=get, set=set , getinv=getinv, setinv=setinv,modified=modified)
    class(tmp) = "cacheMatrix"
    tmp
}

## cacheSolve(x,...)
# 
# Determines if the matrix has been "dirtied" by any of
# the last modifications, and if so, then calculates the inverted
# matrix with "solve()" , sets it and then returns it.
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    if( x$modified() ) {
        retval <- x$setinv( solve(x$get(), ... ) )
    } else {
        message("Nonmodified\n")
        retval <- x$getinv()
    }
    retval
}

#--------------- EXTRA DEBUGGING FUNCTIONS -------------------

# Pretty prints out a cacheMatrix as if
# it was a regular matrix. Debugging friendly
print.cacheMatrix = function(obj) {
    print(obj$get())
}

# [] operator for accessing the cacheMatrix, that
# just passes the [] operator onto the internal matrix
#
`[.cacheMatrix` = function(obj,...) {
    obj$get()[...]
}

# []<- operator for allow us to modify the contents
# of the cacheMatrix. Does some basic argument checking
# and then passes along the arguments to the internal
# matrix setter. Not as robust as other [<- operator functions
`[<-.cacheMatrix` = function(obj,i,j,..., value) {
    args <- list(...)
    ## printf("args was %s\n", class(args))
    ## printf("i was %s",class(i))
    tmp <- obj$get()
    if( ! missing(j) ) { 
        ## printf("j was %s\n",class(j))
        tmp[i] <- value
        obj$set(tmp)
    } else {
        ## printf("j was missing\n");
        tmp[i,j] <- value
        obj$set(tmp)
    }
    obj
}
