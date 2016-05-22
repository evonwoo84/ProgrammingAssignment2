#
# This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #assign value(matrix) to m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get matrix m
  get <- function() x
  
  #calculate inverse of matrix using solve
  #Note** - not all matrixes have inverses
  setinverseMatrix <- function(a) m <<- solve(a)
  
  #get the matrix
  getInverseMatrix <- function() m
  
  #return a list
  list(set = set, get = get,
       setinverseMatrix = setinverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


#
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
#
cacheSolve <- function(x, ...) {
  #step1: get the inverse of the matrix
  m <- x$getInverseMatrix()
  
  #step2: check if inverse already exist in cache
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  
  #step3: otherwise, calculate the inv of the matrix and return the matrix
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data)
  x$setinverseMatrix(m)
  m
  
}
