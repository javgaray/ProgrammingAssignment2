## The first function makeCacheMatrix initializes a matrix and 
## gets the inverse of the matrix x
## The second function gets the cached data of the previous one
## and uses it or solves the matrix if it has not been done

## In this function we create the matrix, caches the data and solves it

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y){
              x <<- y
              m <<- NULL
              }
  
              get <- function() x
              setinverse <- function(solve) m <<- solve
              getinverse <- function() m
              list(set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
  
}


## This function get date from the above and solves the matrix if not done

cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          if(!is.null(m)){
          message("getting cached data")
          return(m)
          }
  
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
  
          m
}