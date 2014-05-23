makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setSo <- function(so) r <<- so
        getSo <- function() r
        list(set = set, get = get,
             setSo = setSo,
             getSo = getSo)
}




cacheSolve <- function(x, ...) {
        r <- x$getSo()
        if(!is.null(r)) {
                message("getting cached matrix")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setSo(r)
        r
}
