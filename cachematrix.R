## These two functions work together to cache matrix inverse operation:
##   makeCacheMatrix creates the matrix for caching its inverse
##   cacheSolve checks cached inverse before calling solve to invers matrx

## This function create matrix for caching its inverse by following steps:
##   1. set/get matrix
##   2. set/get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    # clear temp matrix 
    m <- NULL
    # set cached matrix value 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get cached matrix
    get <- function() x
    # set matrix inverse
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function cache matrix inverse operation:
##   1. check whether the matrix is invertible
##   2. check whether the inverse matrix exist
##   3a. get inversed matrix if it exists
##   3b. call solve() to inverse matrix and save result in cache
#
## Ref: http://r.789695.n4.nabble.com/Singularity-problem-td3382093.html
##      https://stat.ethz.ch/pipermail/r-help/2006-November/117087.html
##      http://stackoverflow.com/questions/10826816/fast-method-to-check-if-a-matrix-is-singular-non-invertible-det-0

cacheSolve <- function(x, ...) {
    # test whether matrix is invertible
    #   if and only if det() is not zero
    if(det(x$get()) == 0) {
        message("matrix is not invertible!")
        return(x$get()) 
    }
    
    # try to get the matrix inverse
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # Be aware of limitation of slove()
    # http://stackoverflow.com/questions/21364060/calculate-inverse-of-a-non-square-matrix-in-r
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
