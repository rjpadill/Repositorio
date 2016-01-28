## FUNCTION CACHE MATRIX
# four functions encapsulated in a list
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matri
makeCacheMatrix <- function(x = matrix()) {
	matriz <- NULL
        set <- function(y) {
                x <<- y
                matriz <<- NULL
        }
        get <- function() x
        setinv <- function(inv) matriz <<- inv ##function that inverse the matrix
        getinv <- function() matriz ##function that returns the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## CACHE SOLVE: Return data from cache memory, inverse matrix 
  # Get the current state of the inverse 
cacheSolve <- function(x, ...) {        
	matriz <- x$getinv()
        if(!is.null(matriz)) {
                message("Obteniendo los datos de la memoria caché...") # Simply return the computed inverse	
                return(matriz)
        }
        data <- x$get()
        matriz <- solve(data, ...) 
        x$setinv(matriz)
        matriz
}
