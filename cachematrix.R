## create matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
+ inv<-NULL
  ## get/set definitions for matrix
+ set<-function(y){
  + x<<-y
  + inv<<-NULL
}
  + get <-function() x
  + setinv<-function(inverse) inv<<-inverse
  + getinv<-function() inv
  ## list of return functions
  + list(set=set, get=get, setinv=setinv,getinv=getinv)
}


##bring out cached solution of the inverse
cacheSolve <- function(x, ...) {
  +  inv <- x$getinv()
  ##return matrix inverse from previous command
  +  if(!is.null(inv)) {
    +    return(inv)
}
  +  data <- x$get()
  +  inv <- solve(data, ...)
  +  x$setinv(inv)
}	
