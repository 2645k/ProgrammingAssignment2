## Put comments here that give an overall description of what your
## functions do
## This is written fro the course assignment
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL                             ## initialize invMat as NULL later will assigned matrix inverse 
  set <- function(y) {                    ## define the set function to set new 
    x <<- y                             ## matrix in parent environment
    invMat <<- NULL                        ## if there is a new matrix, reset invMat to NULL
  }
  get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
  setInverMat <- function(inverse) invMat <<- inverse  ## assigns value of invMat in parent environment
  getInverMat <- function() invMat                     ## gets the value of invMat where called
  list(set = set, get = get, setInverMat = setInverMat, getInverMat = getInverMat)  ## this is needed in order to refer 
                                                                                    ## the functions with the $ operator
}


## Write a short comment describing this function
## This function gives the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already present and matrix is unchanged,
## then cacheSolve will give the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverMat()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInverMat(invMat)
  invMat
}
