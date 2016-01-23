## cachematrix.R 
## for 2nd programming assignment, Coursera R Programming, Jan. 2016  gpstan

## develop two functions to address computational cost of matrix inversion. 
## 1. makeCacheMatrix creates special "matrix" object to cache matrix inverse
## 2. cacheSolve returns the inverse of the makeCacheMatrix object, either
##    computing the inverse for a new or changed matrix object, or 
##    returning the prior inverse computation for an unchanged matrix object.

## The pair of functions take advantage of R's lexical scoping rules to assign
## (and change) values set in other (function) environments. 
##    key free variables
##          inv - the inverse of matrix x ; set to NULL initially and on
##                change in matrix x done through function setm
##          x   - value of maxtrix x

## the functions are used together as follows:
##    for matrix x, process using makeCacheMatrix and assign to a list variable
##    process the list variable through cacheSolve to return the matrix inverse

## function makeCacheMatrix builds a list of functions for a given matrix
## to save computational effort in determining the inverse of a matrix, when 
## that inverse has already been determined.
## makeCacheMatrix builds a list of four function objects:
##    setm(y) can be used in the form x$setm(y) to change the value of a matrix
##          that has been processed through makeCacheMatrix
##    getm() is used in the form x$getm() to return the value of a matrix x
##          that has been processed through makeCacheMatrix
##    setinv(inverse) assigns the passed variable to the free variable inv
##          where inv is used to store the inverse of the matrix x
##    getinv() will return the value of the free variable inv


makeCacheMatrix <- function(x = matrix()) {
      ## set value of inv as NULL : free variable defining inverse of matrix x
      inv <- NULL
      
      ## function to (re)set value of matrix x , assign to setm list object
      setm <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## get value of matrix, assign to getm list object
      getm <- function() {x}
      
      ## set value of inverse
      setinv <- function(inverse) {inv <<- inverse}
      
      ## get value of inverse
      getinv <- function() {inv}
      
      ## return list of functions as value of makeCacheMatrix
      list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}


## function cacheSolve returns the inverse of matrix x, either from a
## previous solutions or by determination using the solve function 
## cacheSolve first checks to see if an inverse solution exists in x. 
## If so, it returns the previously computed inverse matrix.
## If not, it solves for the inverse of matrix x, stores into the free
## variable inv, and returns the inverse value

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## get inverse matrix from free variable inv created in makeCacheMatrix
      inv <- x$getinv()
      
      ## if inv exists, (means inverse has been previously calculated) 
      ## return the value for the inverse matrix
      if(!is.null(inv)){
            message("using cached inverse value")
            return(inv)
      }
      
      ## else get matrix, calculate inverse, save to "special" matrix object
      new_matrix <- x$getm()
      inv <- solve(new_matrix)
      x$setinv(inv)
      
      ## return inverse matrix
      inv
}
