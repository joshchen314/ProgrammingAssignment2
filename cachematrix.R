makeCacheMatrix <- function(x = matrix()) {
  # this is a InverseMatrix caculator with caching
  invM = NULL
  setM = function(y) {
    x <<- y
    invM <<- NULL
  }
  getM = function() x
  setinvM = function(inverse) invM <<- inverse 
  getinvM = function() invM
  list(setM=setM, getM=getM, setinvM=setinvM, getinvM=getinvM)
}

cacheSolve <- function(x, ...) {
  # caching!!!
  invM = x$getinvM()
  if (!is.null(invM)){
    message("get cached!!!")
    return(invM)
  }
  mat.data = x$getM()
  invM = solve(mat.data, ...)
  x$setinvM(invM)
  return(invM)
}
