## The functions below not only calculate inverse of the matrix but also 
## allow less computation by caching an already calculated inverse and retrieving it.
## Assumption: The matrix is assumed to be square and invertible, i.e. A*A^(-1) = A^(-1)*A = I
## Comments are inserted in the code for better clarity


## The following function creates a matrix and sets the initial value of the inverse
## to be a null matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  
  # sets default inverse value to be null. Due to operator being <<- below
  # inV first looks for value in the parent environment and then in the current environment.
  # if there is a valid value in the current environment, then it uses the new value
  
  setinverse <- function(inverse) inv <<- inverse
  
  # Gets initial value as NULL but stores calculated value in CacheSolve here
  
  getinverse <- function() inv
  
  # Convert to recursive to use $ operands
  
  list(get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## The function below caclulates the inverse of the matrix if it does not already exists. If
## the inverse exists, then it does not recalculate the inverse, instead it uses the
# previously stored value if the original matrix has not changed
# The function below is self explanatory. The inverse derived using the solve and
# the value is stored using setinverse function and used as the cached value

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
