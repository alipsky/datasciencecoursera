makeCacheMatrix <- function(x = matrix()) {
  #same format as course example first, init value null
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  #Use get function to select matrix
  get <- function() x
  setinverse <-function(inverse) z <<- inverse
  getinverse <- function() z
  list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
  z <- x$getInverse() 
  if(!is.null(z)) {
    message("getting cached matrix")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setInverse(z)  
  z                
}
