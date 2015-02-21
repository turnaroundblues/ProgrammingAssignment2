## makeCacheMatrix and cacheSolve allow to calculate the inverse of a matrix and to cache it,
## so that subsequent function calls to calculate the inverse of the same matrix simply return 
## the already calculated matrix from the cache, without wasting time on duplicate calculations

##
## makeCacheMatrix takes a matrix (assumed invertible, so no check on invertibility - following instructions)
## and transforms it into an object including "inv" which is initially NULL, and is set to the matrix inverse
## once cacheSolve is called
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmtrxinv <- function(mtrxinv) inv <<- mtrxinv
  getmtrxinv <- function() inv
  list(set = set, get = get,
       setmtrxinv = setmtrxinv,
       getmtrxinv = getmtrxinv)  
}

##
## cacheSolve calculates the inverse of a matrix from the related object constructed by makeCacheMatrix,
## and saves the inverse (caches it) in that object.  In subsequent calls for the same matrix object, 
## cacheSolves simply retrieves the already calculated inverse and returns it.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmtrxinv()
  if(!is.null(m)) {
    message("getting cached inversematrix")
    return(m)
  }
  inputmatrix <- x$get()
  m <- solve(inputmatrix, ...)
  x$setmtrxinv(m)
  m
}

