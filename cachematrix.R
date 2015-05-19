## mat

## To create a matrix with cache


makeCacheMatrix <- function(x = matrix()) {
  ## Initializing m
  m<-NULL
  ## Defining funtions to access the matrix and cache
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setRev<-function(Re) m<<-Re
  getRev<-function() m
  ## Return the list of functions as the key to the matrix and cache
  list(set=set, get=get, setRev=setRev, getRev=getRev)
}


## To Calculate the inverse of the matrix with cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getRev()
  ## If cache exists, return cache
  if(!is.null(m)){
    message("caching")
    return(m)
  }
  ## If not exists, calculate and save in cache
  data<-x$get()
  ## call solve() to get the inverse of matrix
  m<-solve(data, ...)
  ## save in cache
  x$setRev(m)
  ## Return result
  m
}
