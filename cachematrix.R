## Assignment: Caching the Inverse of a Matrix


## TThis function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  inverse <- NULL
  
   set <- function( matrix ) {
    m <<- matrix
    inverse <<- NULL
  }
  
   get <- function() {
     m
  }
  
   setInverse <- function(inverse) {
    inverse <<- inverse
  }
  
   getInverse <- function() {
       inverse
  }
 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  
 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
 
  data <- x$get()
  
 
  m <- solve(data) %*% data
  

  x$setInverse(m)
  
 
  m
}
