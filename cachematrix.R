## makeCacheMatrix and cacheSolve allow to calculate the inverse of a matrix and to cache it,
## so that subsequent function calls to calculate the inverse of the same matrix simply return 
## the already calculated matrix from the cache, without wasting time on duplicate calculations

##
## makeCacheMatrix takes a matrix (assumed invertible, so no check on invertibility - following instructions)
## and transforms it into an object including "inv" which is initially NULL, and is set to the matrix inverse
## once cacheSolve is called
##
makeCacheMatrix <- function(x = matrix()) {
  ## when first called (before the first call to cacheSolve), set the inverse to NULL
  inv <- NULL
  ## get retrieve the matrix itself from the list object created by makeCacheMatrix
  get <- function() x
  ## setmtrxinv caches the inverse 
  setmtrxinv <- function(mtrxinv) inv <<- mtrxinv
  ## getmtrxinv retrieves the inverse from the cache
  getmtrxinv <- function() inv
  
  list(get = get,
       setmtrxinv = setmtrxinv,
       getmtrxinv = getmtrxinv)  
}

##
## cacheSolve calculates the inverse of a matrix from the related object constructed by makeCacheMatrix,
## and saves the inverse (caches it) in that object.  In subsequent calls for the same matrix object, 
## cacheSolves simply retrieves the already calculated inverse and returns it.
## 
cacheSolve <- function(x, ...) {
  ## Return the contents of the cache
  m <- x$getmtrxinv()
  ## if cache is not NULL, simply returns the previously calculated inverse
  if(!is.null(m)) {
    message("getting cached inversematrix")
    return(m)
  }
  ## otherwise, calculate the inverse and cache the inverse
  inputmatrix <- x$get()
  m <- solve(inputmatrix, ...)
  x$setmtrxinv(m)
  m
}

