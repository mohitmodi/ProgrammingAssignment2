#Creates a special "matrix", which is really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the mean
#4.  get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  
  invMtx <- NULL
  
  set <- function(y) {
    x <<- y
    invMtx <<- NULL
  }
  
  get <- function() x
  
  setInvMtx <- function(inverseMtx) invMtx <<- inverseMtx

  getInvMtx <- function() invMtx
  
  list(set = set, get = get, setInvMtx = setInvMtx, getInvMtx = getInvMtx)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated, return cached inverse of the matrix.
cacheSolve <- function(x, ...) {
  invMtx <- x$getInvMtx()
  
  if (!is.null(invMtx)) {
    message("getting cached inverse matrix")
    return(invMtx)
  }
  
  
  mtx <- x$get()
  invMtx <- solve(mtx, ...)
  
  # Cache the inverse
  x$setInvMtx(invMtx)
  invMtx
}
