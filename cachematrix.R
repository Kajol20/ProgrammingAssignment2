## caching inverse of a matrix

## makeCacheMatrix function creates a matrix and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function() x
  setinv<-function(i) inv<<-i
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function returns inverse of the matrix using cache if it's available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i=x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat<-x$get()
  i<-solve(mat, ...)
  x$setinv(i)
  i
}
