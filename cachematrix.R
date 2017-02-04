
#The first function, 'makeCacheMatrix' creates a special "vector"

makeCacheMatrix <- function(x = matrix()){
  
  ## The function 'makeCacheMatrix' takes a squaremetrix as an input
  ## and creates a vector whih is a specil list 
  ##containing a functions:
  ##          'setMat' to set the value of the matrix
  ##          'get Mat' to get the value of the matrix
  ##          'setMatInv' to set the value of the inverse
  ##          'getMatInv' to get the value of the inverse
  
  ## The function returns the list of the functions
  
    Mat <- NULL
    setMat <- function(y) {
      x <<- y
      Mat <<- NULL
  }
  getMat <- function() x
  SetMatInv <- function(solve) Mat <<- solve
  GetMatInv <- function() Mat
  
  list(setMat = setMat, getMat = getMat,
       SetMatInv = SetMatInv,
       GetMatInv = GetMatInv)
}


## The following function calculates the inverse of of the special "matrix" 
##created with the above function

cacheSolve <- function(x, ...) {

  ## The function takes as na input a list 'x' of length 4 from the output
  ## function ' makeCacheMatrix' it
  ## Checks from the list if the inverse has been calculated if not, 
  ## takes first element of the list, which the matrix used, to 
  ## calculate the invierse and sets the value of the inverse in the cache via 
  ## 'setMatInv'
  
  ## The function returns the inverse of the input matrix.
  
  m <- x$GetMatInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  Matdata <- x$getMat()
  m <- solve(Matdata, ...)
  x$SetMatInv(m)
  m
}
