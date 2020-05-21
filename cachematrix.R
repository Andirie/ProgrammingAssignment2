## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invMat<- NULL                                     ##holds the value of inverse matrix
set<- function(y){                                ##set the value of matrix
  x<<-y                                           ##assign a value to an object in an environment that is different from the current environment
  invMat<-NULL
}
get<- function()x                                 ##get the value of matrix
setInverse<-function(inverse) invMat<<-inverse    ##set the value of inverse matrix
getInverse<- function()invMat                     ##get the value of inverse matrix
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated,then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {                        
        ## Return a matrix that is the inverse of 'x'
  invMat<- x$getInverse()
  if(!is.null(invMat)){
    message("getting cached matrix")
    return(invMat)
  }
  data<- x$get()
  invMat<-solve(data,...)
  x$setInverse(invMat)       ##sets the value of the inverse matrix in the cache via the setInverse function
  return(invMat)
}
