## The makeCacheMatrix and the cacheSolve functions together cache
## the inverse of a matrix so that if the value of the inverse is
## required again, it can be retrieved from the cache instead of
## recalculating it thus saving computational time

## The makeCacheMatrix function creates a "special" matrix which
## is essentially a list of four functions to set and retrieve 
## the values of the matrix and their respective inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inverse) i<<-inverse
  
  getinverse<-function() i
  
  list(set=set,get=get,
       setinverse=setinverse, getinverse=getinverse)

}

## The cacheSolve function solves for the inverse of a matrix.
## It does so by getting the matrix through the makeCache matrix.
## It first checks whether the inverse has already been calculated
## or not. If it has already been calculated, it returns the cached
## inverse, else it solves for the inverse, stores the inverse in the
## list created through makeCacheMatrix, and returns the inverse of
## the matrix.

cacheSolve <- function(x, ...) {
  
  i<-x$getinverse()
  
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  
  data<-x$get()
  
  i<-solve(data)
  
  x$setinverse(i)
  i
}
