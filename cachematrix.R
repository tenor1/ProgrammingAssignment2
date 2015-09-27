## makeCacheMatrix function store special matrix object and inverted
## in cache and give get, set, getinv, setinv function access to data

## x <- makeCacheMatrix - start testing with default

makeCacheMatrix <- function(x = matrix(c(1,2,2,2),2,2)) {
  inv <- NULL  # inverted matrix
  set <- function(y) { # new base matrix setting
    x <<- y
    inv <<- NULL
  }
  get <- function() x     # base matrix get
  setinv <- function(inm) inv <<- inm # set inverted
  getinv <- function() inv # get inverted
  
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve function give access to cached matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
   if(!is.null(inv)) {
      message("=== getting cached data ===")
      return(inv)
   }
   m <- x$get()
   inv <- solve(m)
   x$setinv(inv)
   inv     ## Return a matrix that is the inverse of 'x'
}
