## Las dos funciones en conjunto calculan la inversa de una matriz y las almacenan en 
## cache para poder usarlas en otro momento.

## Esta función crea un objeto matriz que puede almacenar en caché su inverso

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Esta funcion calcula la inversa de una matriz que es devuelta por la funcion 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  ## Analiza si la matriz ya esta en cache y la devuelve de ser el caso
  if(!is.null(s)) {        
    message("consiguiendo datos del cache")
    return(s)
  }
  ## Si no existe en cache la inversa de la matrix la calcula   
  
  ## Almacena en la variable data la variable x mediante la funcion get
  data <- x$get()
  ## Cacula la inversa de data y almacena en la variable s
  s <- solve(data, ...)
  ## Envia el resultado de la inversa(s) a la funcion set para almacenarla en x
  ## y almacenarla en cache mediante la funcion makeCacheMatrix
  x$setsolve(s)
  ## Muestra el resultado de la inversa de la matriz
  s
}