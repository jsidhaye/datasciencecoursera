## This function creates a special "matrix" object that can cache its inverse.

## Initialise a matrix x and inv which is inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL 
  ## Set values to x and inv in this environment 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ## get value of x form paretn environment
  get<-function() x
  ## Set the value for the inverse inv in this environment
  setinv <- function(inverse) inv <<- solve
  ## get the value for inverse recently set
  getinv <- function(inverse) inv
  ## Make a new list variable with all set and get values for x and inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Use the list obtained from MakeCacheMatrix vector
cacheSolve <- function(x, ...) {inv<-x$getinv()

## if inv has a value then print message getting cached data
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
}
##use the value of the matrix from get() in the list
data <- x$get()
## calculate inverse using this value
inv <- solve(data, ...)
x$setinv(inv)
## Return a matrix that is the inverse of 'x'
inv
}
