##programm assignement 2

## Calculate and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }


## return the inverse of the matrix ; solves only 
## if it has not been solved before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
   if(!is.null(m)) {
    message("getting cached inversed matrix")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...) ##solve : gives the inverse of the matrix
  x$setinv(m)
  return(m)
  }

