## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## create a special matrix  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) m <<- Imatrix
  getImatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}
## Write a short comment describing this function
## parameter: x is a matrix
## if x is square then solve inverse x
## if not my function inverse do this.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  myMatrix <- x$getImatrix()
  if(!is.null(myMatrix)){
    message("Cached data found. Getting result... Done.")
    return(myMatrix)
  }
  else {
    message("No cached data found. Calculating inverse matrix...")
    mydata <- x$get() # obtains matrix from object x
    if(nrow(mydata) == ncol(mydata)){
      myMatrix <- solve(mydata) # finds inverse matrix    
    }
    else{
      myMatrix <- inverse(mydata)
    }
    x$setImatrix(myMatrix) # assigns resulting inverse matrix to object x
    message("Done.")
    return(myMatrix)
  }
}

inverse <- function(X, tol = sqrt(.Machine$double.eps))
{
  ## Generalized Inverse of a Matrix
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
  return (dnx)
}