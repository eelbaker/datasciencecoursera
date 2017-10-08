##makeCacheMatrix and cacheSolve are a pair of functions to store and access a cached value of a matrix's inverse.
##makeCacheMatrix creates a list of functions that reference an environment containing the matrix
##cacheSolve takes this list, and checks to see if there is a stored inverse before using solve()

## makeCacheMatrix creates a list of 4 functions with references to an environment containing matrix x and its stored inverse.
## set takes an argument and assigns its value to matrix x
## get returns matrix x
## setinv takes an argument and sets stored inverse inv
## getinv returns inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {x <<- y}
  get <- function(){x}
  setinv <- function(inver) {inv <<- inver}
  getinv <- function() {inv}
  list( set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve takes a list from makeCacheMatrix as its argument.  
## It first checks for a stored inverse.  If one exists, it prints a message, and returns it.
## If not, it calculates the inverse with solve(), stores that inverse with setinv(), and returns it

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
