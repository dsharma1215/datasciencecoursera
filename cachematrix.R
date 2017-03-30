##Following 2 functions demonstrate lexical scoping feature of R language
##Aim is to return and cache inverse of an input matrix
##If same matrix is passed again, do not calculate
## inverse agin and retrive it from cache along with message 
## "Data retrived from Cache"
## If a new matrix is passed to the function then calculate its inverse, print
## out the inverse matrix and cache it for next execution of function



##To test the functionality do the following
## 1. Create an invertiable matrix e.g
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## 2. Pass this matrix as an argument to function makeCacheMatrix
## my_mat <- makeCacheMatrix(m1)
## 3. Pass the result to secong function cacheSolve()
## n1 <- cacheSolve(my_mat)
## n1 should be the inverse matrix of our m1 matrix
## 4. To verify calculate m1%*%n1 - result should be a square matrix with 
##    0 and 1s ( 1s would be present diagonally in the resultant matrix)




#Function#1
## makeCacheMatrix takes input as a matrix 'x' (assumed to be invertiable)
## and retuns following list of functions to be used by CacheSolve function later
##1. set() -   This makes value of 'x' available in its parent environment
##             and resets/clears the value of inverse matrix 'invm'
##2. get() -   This gets the value of input matrix 'x' to CacheSolve function
##3. setinv() -This passes the value of inverse matrix 'invm' to the parent 
##             environment  
##4. getinv() -This helps get the correct value of inverse matrix "invm" to
##                the cacheSolve function
## At the end these 4 functions are named (same name as function)
## and passes as a list so that they can be called with '$' operator

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL            ##initialize inverse matrix
  set <- function(y) {
    x <<-y               ## make matix 'x' available in parent environment
    invm <- NULL         ##initialize/clear inverse matrix
  }
  get <- function() x
  setinv <- function(solve) invm <<- solve
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




##Function # 2
## Return a matrix that is the inverse of 'x'
##CacheSolve function takes a list argument from make CacheMatrix function above
## each element of list is a either a get or set function
##It uses getinv() function from the list to see if it already has a value
##(inverse already calculated). If yes, the function prints out "Data retrived
## from cache" and print out already stored inverse matrix.
##if no previous inverse matrix exists then get() function from input list is 
##called to get the new input matrix and inverse is calculated via it R
##function solve(). This new inverse matrix is passed via setinv() function
##for next execution check and the inverse matrix is printed out.

cacheSolve <- function(x, ...) {
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("Data retrived from cache")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data,...)
  x$setinv(invm)
  invm
}





