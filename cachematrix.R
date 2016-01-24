## FUNCTION CACHE MATRIX
makeCacheMatrix <- function(x = matrix()) {
	matriz <- NULL
        set <- function(y) {
                x <<- y
                matriz <<- NULL
        }
        get <- function() x
        setinv <- function(inv) matriz <<- inv
        getinv <- function() matriz
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## CACHE SOLVE: Return data from cache memory, inverse matrix 

cacheSolve <- function(x, ...) {        
	matriz <- x$getinv()
        if(!is.null(matriz)) {
                message("Obteniendo los datos de la memoria caché...")
                return(matriz)
        }
        data <- x$get()
        matriz <- solve(data, ...)
        x$setinv(matriz)
        matriz
}
