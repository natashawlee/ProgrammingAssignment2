##makeCacheMatrix creates a special matrix object that can cache its inverse
##cacheSolve computes the inverse of the special matrix created by makeCacheMatrix.

##makeCacheMatrix 
#1. Sets up the matrix
#2. Gets the matrix
#3. Sets the inverse
#4. Gets the inverse

makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) m<--inverse
  getinverse<- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

#cacheSolve calculates the inverse of the special matrix from makeCacheMatrix
#However, it first checks if the inverse has been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets inverse in the cache via the setinverse function.

cacheSolve<-function(x,...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
