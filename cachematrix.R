## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix generate a list of funtions: set, get, setinverse and getinverse in order to calculate
## and store in cache the inverse of a matrix (x). 
## Ex:
## a <- replicate(10, rnorm(10)) 
## storematrix<-makeCacheMatrix(a)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inversed matrix stored in makeCacheMatrix for an object makeCacheMatrix
## Ex:
## a <- replicate(10, rnorm(10)) 
## storematrix<-makeCacheMatrix(a)
## c<-cacheSolve(storematrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
