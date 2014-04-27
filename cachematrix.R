## These two functions permit to calculate the inverse of a matrix.
## Estas dos funciones permiten calcular la inversa de una matriz.

## makeCacheMatrix is a function that create a special matrix, that really is a list that contains all the arguments needed to calculate the matrix's inverse.
## makeCacheMatrix es una funcion que crea una matriz especial, que realmente es una lista que contiene todos los argumentos necesitados para calcular la inversa.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {                      ## set matrix's values / establece los valores de la matriz           
            x <<- y
            m <<- NULL
      }
      get <- function() x                       ## get matrix's values / obtiene los valores de la matriz
      setsolve <- function(solve) m <<- solve   ## set matrix's inverse / establece la inversa de la matriz
      getsolve <- function() m                  ## get matrix's inverse / obtiene la inversa de la matriz
      list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)    ## list that contains the arguments to calculate matrix's inverse / lista que contiene los argumentos para calcular la inversa de la matriz
}


## cacheSolve checks wheter the inverse has already been calculated and retrieve from the cache memory, otherwise it calculates through setsolve
## cacheSolve comprueba si la inversa ya se ha calculado y la recupera de la memoria cache, caso contrario la calcula a traves de setsolve 

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x' / devuelve una matriz que es la inversa de 'x'
      
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
