## These functions are used for when one needs to invert a matrix
## and get the result multiple times without recalculating.
## "makeCacheMatrix" sets up the matrix and needed functions
## "cacheSolve" returns the inverted matrix

## returns a list that can be used with the "cacheSolve" function
## a matrix can be passed or set later by passing it to the "Set" 
## function returned in the returned list. The value of the matrix
## can be  retrieved by the "Get" function returned in the list.

makeCacheMatrix <- function(x = matrix()) {
  Matrix <- x
  Inverse <- NA
  Set <- function(value){
    Matrix <<- value
    Inverse <<- NA
  }
  Get <- function(){
    Matrix
  }
  SetInv <- function(value){
    Inverse <<- value
  }
  GetInv <- function() {
    Inverse
  }
  return(list("Set" = Set, "Get" = Get, "SetInv" = SetInv, "GetInv" = GetInv))
}



## runs the solve function on a list generated from the "makeCacheMatrix"
## function and stores the result so future calls to this function will
## not have to run solve unless the data has changed.

cacheSolve <- function(x, ...) {
  if(length(x) > 1 || is.na(x$GetInv())){ ##the length condition is to supress
                                          ##a warning that is.na generates for
                                          ##things with alength greater than 1
    x$SetInv(solve(x$Get(),...))
  }
  return(x$GetInv())
}
