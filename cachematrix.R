## makeCacheMatrix receives a matrix as an input
## and returns a special list that stores a matrix
## and cache's its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
## checks if the matrix is the same then
## returns the cached copy

cacheSolve <- function(x, ...) {
  ## i is the inverse matrix of the one passed as input, x
  i <- x$getinv()
  ## o is the original matrix extracted from x
  o <- x$get()
  
  ## if A and AI (inverse of A) are 2 matrixes
  ## then A %*% AI == AI %*% A
  ## we can use that logic to identify that the matrix o
  ## has the correct inverse in i
  
  if(!is.null(i) && (o %*% i == i %*% o)) {
    message("getting cached data")
    return(i)
  }
  
  ## we do not have any cached copy, need to calculate one
  ## assumptions are o is a square and invertible matrix
  calcinv <- solve(o)
  x$setinv(calcinv)
  calcinv
}
