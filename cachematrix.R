## This function initiates two variables to cache the matrix data and 
## inverse data, then checks if the inverse data exists before computes it

##Intiate inverse data variable and cache both matrix and inverse data

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) { 
        x <<- y
        iv <<- NULL 
    }
    get <- function() x
    setiv <- function(solve) iv <<- solve
    getiv <- function() iv   
    list(set = set, get = get,
         setiv = setiv,
         getiv = getiv)
}


## to check if inverse data exists before compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getiv()  ## Check if inverse data exists
  if(!is.null(iv)) { 
      message("getting cached data")
      return(iv)
  }
  m <- x$get() ## Get the matrix data and compute the inverse
  iv <- solve(m, ...)
  x$setiv(iv) ## Set to cache
  iv
}
