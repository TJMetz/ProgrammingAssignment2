## makeCacheMatrix stores an input matrix x and provides an environment for
## caching the output from another function, such as the matrix inverse created 
## by the function cacheSolve.
##
## Returns a list of functions: getmatrix() returns the cached value, 
## get() returns the input data matrix, setmatrix() stores the result from
## the calling function (e.g. cacheSolve), and set(y) resets the data matrix to
## y and the cached value m to NULL. 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get<-function() x
        setmatrix<-function(inv) m<<- inv
        getmatrix<-function() m
        list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}

## cacheSolve calculates inverse of matrix x, first checking the cache defined
## by makeCacheMatrix to see if it exists. If it exists, the inverse matrix m
## is returned; otherwise it is calculated, cached in variable m, then returned.
## set() is not called.

## Example:
## data <- matrix(rnorm(25), 5, 5)
## x <- makeCacheMatrix(data)
## inverse <- cacheSolve(x)

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached matrix inverse")
                return(m)
        }
        matrix<-x$get()
        message("calculating matrix inverse")
        m<-solve(matrix, ...)

        x$setmatrix(m)
        m
}
