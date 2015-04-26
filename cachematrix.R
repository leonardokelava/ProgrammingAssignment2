##This function creates a list of functions, which can then  be called by name. 
## $sets the new value for input matrix x and resets the inverse matrix.
## $get is a function which just gets the value of input matrix x.
## $setsolve gives the inverse matrix and assigns its value to  i.
## $getsolve gives the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <-function(solve) i <<-solve
  getsolve <- function() i
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This functions gets the inverse i of input matrix x if something is stored in i variable.
## If nothing is stored, it calculates the  inverse matrix and assigns it to the i variable by calling to x$get and x$setsolve functions.  

cacheSolve <- function(x, ...) { 
  i <-x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <-solve(data,...)
  x$setsolve(i)
  i  
        
}
