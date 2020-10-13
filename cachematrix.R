## The following two functions take a Matrix, calculate its inverse and cache the inverse

## this function create a special object makeCacheMatrix that:
#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of the Inverse
#4.  get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set_matrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache using the `get_inverse´function and skips the computation. Otherwise, it calculates the inverse of
## the given Matrix with the `solve` function and sets the value of the inverse in the cache via the `set_inverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get_matrix()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
