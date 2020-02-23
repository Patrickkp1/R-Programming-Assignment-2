makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    x <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list (set = set, get = get, 
        setmatrixinverse = setmatrixinverse, 
        getmatrixinverse = getmatrixinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinverse
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setmatrixinverse(m)
  m
}



