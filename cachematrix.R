


##This function takes a matrix, sets it in the global environment to be called later when the function ends.
##It sets it equal to NULL every time the function is called again, if the inverse has not been cached. 
##If the inverse has been cached using the cacheSolve matrix, it return the value using the $getsolve argument.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}








## Write a short comment describing this function
## Without the makeCacheMatrix function, cache solve is incomplete. When it is called, it 
##gets the getsolve from the input object, then it checks if the input object is NULL. If it is not, 
##it calculates the inverse and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
