## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  i<-NULL ## Initialize the inverse property
  
  ## Method to set the matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  
  ## Method the get the matrix
  get<-function(){m}
  
  ## Method to set the inverse of the matrix
  setinv<-function(inverse){
    i<<-inverse
  }
  
  ## Method to get the inverse of the matrix
  getinv<-function(){
    i
  }
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setinv(m)
  
  ## Return the matrix
  m
}

##test
f<-makeCacheMatrix(matrix(c(8,5,6,4),2,2))
cacheSolve(f)
