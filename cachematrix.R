## this 2 functions is used to cache the matrix inverse, it can save the 
## computation time if the matrix inverse is used for more than one time.

## create a vector with functions to: get matrix value, set matrix value, get matrix inverse, set matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return reverse of matrix. if it's in catch, return, else calculate and save to catch.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) * data
  x$setinvers(inv)
  inv
}
