#makeCacheMatrix is meant to set and get the value of the matrix 'x'
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL #Initializing Objects
  set <- function(y) {
    x <<- y
    i <<- NULL #Clears any value of i that had been cached before by cacheSolve()
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #allows for use of $ form of extract operator
}
#cacheSolve is meant to set and get the value of the inverse matrix of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data") #If there is a valid cached mean
    return(i) #Returns valid cached mean to its parent environment
  }
  data <- x$get()
  i <- solve(data, ...) #Solves for the inverse matrix
  x$setinverse(i)
  i
}
#Testing Code
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)