## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  the first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to:
#      set the value of the vector
#      get the value of the vector
#      set the value of the mean
#      get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  matrix_Inverse <- NULL #initialize the inverse matrix to null
  # Assign to set the internal function that create internal variables
  set <- function(y) {
    # set is used to assign 2 internal variables
    #Internal variable X is assigned Y for value
    #This is only for the current environment
    x <<- y 
    #Internal variable matrix_Inverse is assigned null for value 
    #This is only for the current environment. 
    #If you do my_matrix$set(x),it is a new "matrix" 
    #and the cache is not utilized
    matrix_Inverse <<- NULL 
    # Basically set reinitialise the matrix_Inverse, 
    
  }
  #get: function to get the current matrix
  get <- function() x #function to get the current matrix
  # setInverse: function to set/calculate  the matrix inverse
  # is invoked using XX$setInverse(solve(test))
  setInverse <- function(inverse) matrix_Inverse <<- inverse
  # getInverse: function to return the matrix inverse
  # will return the matrix inverse if invoked using 
  # xx$getInverse() with xx being set as makeCacheMatrix(x)
  getInverse <- function() matrix_Inverse
  # creates a list of the functions created in makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
# Function calculates the matrix inverse if new matrix, and cache result
# If the same matrix needs to be inverted, the calculations are skipped,
# and the cache value is used and returned instead
# the inverse matrice when calculated is set using 'saved' using 
# x$setInverse(matrix_Inverse).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # statement below get the inverse matrix of x from makeCacheMatrix
  # function. It assigns it to matrix_Inverse
  matrix_Inverse <- x$getInverse()   
  # If matrix_Inverse is not null, then the matrix_inverse 
  # is cached, and do not need to be calculated, it just can be returned
  if(!is.null(matrix_Inverse)) {
    message("getting cached data")
    return(matrix_Inverse)
  }
  # If matrix_Inverse is null, then it is the first time,
  # it has to be calculated
  # data is set to the matrix to inverse
  data <- x$get()
  # matrix_Inverse is assigned to the inverse matrix using the R function solve
  matrix_Inverse <- solve(data, ...)
  # Invoque the setInverse to set the cache
  x$setInverse(matrix_Inverse)
  #matrix_Inverse is returned
  matrix_Inverse
}

