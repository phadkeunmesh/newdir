## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function is very similar to the cached mean function that was given as an example.
##set function sets the matrix,get is used to get the matrix stored. GetInverse fn is used to get hte cached mean and setInverse is used to set the mean.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) i<<-inverse
  getInverse<- function() i
  list(set=set, get=get, setInverse= setInverse, getInverse= getInverse)
  
  
}


## Write a short comment describing this function

##This function first checks whether the inverse has been cached or not . If it has been cached it uses getInverse.
##Else inverse is calculated using solve() and set using setInverse.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse
  if(!is.null(i)){
    message("getting cached inverse")
    return (i)
  }
  else{
    mat<-x$get()
    i<-solve(x,...)
    x$setInverse(i)
    return i
  }
}
