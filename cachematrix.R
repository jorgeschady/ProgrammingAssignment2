#This is the work of Jorge Schady for "R-programming"
## Acknowledgements to Bill Hilton for his explanation which can be found at
### https://class.coursera.org/rprog-009/forum/thread?thread_id=457

## These bundle of functions is used to provide the ability to 
## store inverse matrices in cache memory, through the use of
## lexical scoping

## makeCacheMatrix is a function that takes a matrix as an input
## and stores the value of the original matrix and it's inverse (initially NULL)

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL          #final result object is initialized as NULL           
  set <- function(y) {
    x <<- y                       #sets the objects in the cached memory to NULL 
    inverse_matrix <<- NULL
  }
  get <- function() x             # retrieves the data from the original matrix  
  setinverse <- function(inverse) { 
    inverse_matrix <<- inverse    #sets the value of inverse in cache memory if it has
  }                               #been calculated by cacheSolve()         
  getinverse <- function() inverse_matrix  #returns the value of inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse() #retrieves the value stored in the makeCacheMatrix
  if(!is.null(inverse_matrix)) {
    message("Retrieving cached data") # Warns the user the return value is from cache memory
    return(inverse_matrix)            # exits the function (all has been achieved already)
  }
  data <- x$get()                     # in case nothing is stored in cached memory, 
                                      ##retrieves the original data
  inverse_matrix <- solve(data, ...)  #calculates the inverse of the matrix
  x$setinverse(inverse_matrix)        #sets the value to the cached memory   
  inverse_matrix
}
