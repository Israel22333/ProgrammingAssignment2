## The following two functions use Lexical Scope 
## to preserve a matrix together with its inverse matrix, 
## so that the inverse matrix can be accessed whenever needed, 
## without recomputing it,
## and the computation is done separately 
## only when there is a change in the original matrix



## The following function creates functions and variables that will be used 
## to work with the matrix that will be passed to it as an argument. 
## Returns a list of functions 
## that are return (get) and determine (set) the data: 
## the matrix and its inverse.

## The <<- operator means 
## that the variable must be searched for in the parent's environment,
## That is, in the makeCacheMatrix function environment
## and it is **not** a new variable 
## created for the specific environment of this current function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The next function uses the functions defined in the previous function.
## it checks if a recomputation of an inverse matrix is needed, 
## and if so - calculates the new value 
## and assign it to the 'inv' variable in the first function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
