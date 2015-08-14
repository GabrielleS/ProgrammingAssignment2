## The functions below calculate the inverse of a matrix and
## cache the data. Caching data helps in the quickness and 
## costliness of computations by storing data that has already 
## undergone some sort of function, etc. This way you won't have 
## to loop your functions from the beginning everytime new data is
## added. The solver then looks if the data has in fact been
## cached and if not runs the computation.

## This function creates a special "matrix" object that can cache
## its inverse
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  
  ## A double assignment operator "<<-" modifies the variables in 
  ## the parent levels (single "<-" works on the current level).It
  ## is useful for caching because it saves each steps results. 	
  set<- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInversematrix <- function(solve) m <<- solve
  getInversematrix <- function() m
  list (set = set, get = get, 
        setInversematrix = setInversematrix,
        getInversematrix = getInversematrix)
}


## The function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInversematrix()
  ## The folowing checks if "m" is not null (ie. there is cached
  ## data) it returns the cached data so as not to recalculate
  ## everything.	
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the matrix was not found, this reruns the matrix and 
  ## inverse calculation in the makeCacheMatrix function. 
  data <- x$get()
  m <- solve(data, ...)
  x$setInversematrix(m)
  m 
}
