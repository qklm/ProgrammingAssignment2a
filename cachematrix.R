
makeCacheMatrix <- function(x = matrix()) {
+ inv<-NULL
+ set<-function(y){
  + x<<-y
  + inv<<-NULL
}
  + get <-function() x
  + setinv<-function(inverse) inv<<-inverse
  + getinv<-function() inv
  + invList(set=set, get=get, setinv=setinv,getinv=getinv)
}



cacheSolve <- function(x, ...) {
  +  inv <- x$getinv()
  +  if(!is.null(inv)) {
    +    return(inv)
}
  +  data <- x$get()
  +  inv <- solve(data, ...)
  +  x$setinv(inv)
}	
