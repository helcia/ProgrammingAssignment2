## This function will find the inverse of a matrix by running the Solve function and cache the result.  The makeCasheMatrix will check whether Solve has already been run on that matrix.  If it has, it will skip and move to the next step and will get the next matrix that hasn't been solved, (ie. find the inverse of the function), solve it using casheSolve and cache that result. Cachesolve is the function that finds the inverse of the matrix each time.


## MakeCacheMatrix will check whether Solve has already been run on that matrix.  If it has, it will skip and move to the next step and will get the next matrix that hasn't been solved; it will call the solve function and and operate on the next matrix that hasn't been worked on.  All of these results are cached into a list.

makeCacheMatrix<-function(x = matrix()){
  m<-NULL
  set<- function(y) {
      x <<- y
      m<<-NULL
  }
  get <-function() {
      x
  }
  setsolve <- function(solve) {
      m<<-solve
  }
  getsolve <-function() {
    m
  }
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Cachesolve is the function that finds the inverse of the matrix each time. It gets the next matrix and puts it in "data" then solves it (find the inverse of the matrix) and places it in "m" so it is available to place in the cache using makeCacheMatrix.

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data<-x$get()
    m<- solve(data,...)
    x$ setsolve(m)
  
}