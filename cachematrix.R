## A list of four functions to set and get the matrix and its invese is created
## and are stored as cache in case the inverse of the same matrix is called for again

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(a) inverse <<- a
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )

}


##the inverse of the matrix is geneated by first checking whether it is already available
## in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse1 <- x$getinverse()
  if(!is.null(inverse1)){
    message("getting cached data")
    return(inverse1)
  }
  b <- x$get()
  inverse1 <- solve(b)
  x$setinverse(inverse1)
  inverse1
}

