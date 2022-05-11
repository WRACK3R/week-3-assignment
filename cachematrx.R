##there are two functions makecachematrix,cachesolve
##makecachematrix consist of set,get,setinv,getinv
##library(MASS)is used to calculate inverse of a matrix
makecachematrix <-function (x=matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
get <-function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function(){
  inver<-ginv(x)
  inver%*%x                ##function to obtain inverse of a matrix
}
list(set=set,get=get,
     setinv=setinv,
     getinv=getinv)
}


cachesolve <-function(x, ...) ## for cache data
{
  inv<-x$getinv()
if(!is.null(inv)){message("getting cached data!")
  return(inv)
}
data<-x$get()
inv<-solve(data,...)
x$setinv(inv)
inv ##returns metrix that is inverse of 'x'
}