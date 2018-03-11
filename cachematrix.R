

## makeCacheMatrix sets the value of a matrix, gets the value of the matrix and
## sets the value of the inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inv) i <<- inv
        geti <- function() i
        list(set = set, get = get,
             seti = seti,
             geti = geti)
}


## This function finds the inverse of the matrix, but if the inverse has already been cached displays that

cacheSolve <- function(x, ...) {
        ## find if an inverse of x is alreaded stored. if it is return cached data, if not find inverse
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$seti(i)
        i
}
