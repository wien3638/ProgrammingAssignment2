## makeCacheMatrix: stores 4 functions:
## set(), get(): stores/retrieves stored matrix
## setinv(), getinv(): calculate inverse(solve()) of the
##   stored matrix, or retrieves stored inverse of the matrix
## cacheSolve: calculate inverse of matrix x and return

## makeCacheMatrix: 4 functions
## set(), get(), setinv(), getinv()
## store/retrieve matrix and calculate/retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: calculate and return inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
