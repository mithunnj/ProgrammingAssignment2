## R programming -> Week 3 -> Programming Assignment 2: Lexical

###########################################
## Input: Valid square invertible matrix

## Functonality:
## - setReg(): Set non-inverted matrix data.
## - getReg(): Get non-inverted matrix data.
## - setInv(): Set inverted matrix data.
## - getInv(): Get inverted matrix data.

## Return: Special matrix object that can cache it's inverse.
###########################################
makeCacheMatrix <- function(x = matrix()) {
  
  # Declare inverse matrix with NULL initial value
  invMatrix <- NULL
  
  # Set non-inverted matrix data
  setReg <- function(inputMatrix) {
    x <<- inputMatrix
    invMatrix <<- NULL
  }
  
  # Get non-inverted matrix data
  getReg <- function() {
    return(x)
  }
  
  # Set inverted matrix data
  setInv <- function(inputMatrix) {
    invMatrix <<- inputMatrix
  }
  
  # Get inverted matrix data
  getInv <- function() {
    return(invMatrix)
  }
  
  # Return the methods associated with the makeCacheMatrix object.
  return(list(setReg = setReg, getReg = getReg, setInv = setInv, getInv = getInv))

}


###########################################
## Input: Cache matrix object.

## Functonality:
## - If inversae has been calculated, and the matrix has not
## changed, then return the cached inverse matrix.
## - Otherwise, calculate the inverse matrix, store the data
## and return the inverted matrix result.

## Return: Inverted matrix.
##########################################
cacheSolve <- function(x, ...) {
  
  invMatrix <- x$getInv()
  
  if (!is.null(invMatrix)) {
    print("Identified valid inverse matrix...")
    return(invMatrix)
  }
  
  print("Inverse matrix resulted in NULL.")
  regMatrix <- x$getReg()
  invMatrix <- solve(regMatrix)
  
  x$setInv(invMatrix)
  
  return(invMatrix)
    
}
