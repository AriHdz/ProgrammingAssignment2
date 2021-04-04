##There are two function makeCacheMatrix and cacheSolve, the first one creates 
##a special matrix and the second one computes the invers 


## makeCacheMatrix creates a special "matrix" ans it will be use later

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL    #Initialize as an object to be used later 
  set <- function(y) {
    x <<- y       #Assign the matrix to the x object in the parent environment
    invm <<- NULL #Reset invm to NULL if there is a new matrix
  }
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse 
  #Assign the argument to the value of invm in the parent environment
  getinverse <- function() invm
  list(set = set, #Gives the name 'set' to set() function
       get = get, #Gives the name 'get' to get() function
       setinverse = setinverse, #Gives the name 'setinverse' to setinverse 
       getinverse = getinverse) #Gives the name 'getinverse' to getinverse 
}


## cocheSolve gives the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        #Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)) {
    #Checks to see if the result is NULL
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...) #Compute the inverse
  x$setinverse(invm)
  invm
}
