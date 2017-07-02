## makecachematrix function caches the inverse of a matrix using solve().
 ##cacheSolve uses makecachematrix to calculate the inverse of a matrix. 
 ##If the inverse was calculated alreadt, it will use the value from the cache;otherwise
 ##it calculates the inverse with solve().

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## makeCacheMatrix sets the value of a matrix, gets the value of the matris, and uses solve()
 ##to calculate the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
##makeCacheMatrix uses makeCacheMatrix to calculate inverses for a matrix, but if it has been
 ##calculated already, it will return the value from makeCacheMatrix.  If not, it wil use 
 ##solve() to calculate the inverse.
