## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#   The object of this function is to store a special type of matrix
#   that also holds its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  #Create inverse matrix variable, set to Null
  invM <- NULL
  
  #Function to set matrix
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  
  #Function to get the value of the matrix
  get <- function() x
  
  #Function to set the inverse of the matrix. NOTE! This persumes
  # the matrix has already been solved.
  setInverse <- function(inverse) invM <<- inverse
  
  #Function to return matrix inverse
  getInverse <- function() invM
  
  #Create list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#   The object of this function is to attempt to solve an existing cached
#   matrix object. If the Inverse already exists, it returns the inverse
#   data. If it doesn't, it solves the matrix and sets the data into the
#   created Matrix.

cacheSolve <- function(x, ...) {
  
  # Create new variable and attempt to load inverse matrix data to it
  invM <- x$getInverse()
  
  # Test to see if Inverse Data exists
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  
  # Solve Matrix and set Inverse Matrix data
  data <- x$get()
  invM <- solve(data)
  x$setInverse(invM)
  
  ## Return a matrix that is the inverse of 'x'
  return(invM)
}
