## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s = NULL                               #cached inverse matrix, empty initially
  set = function(y){
    x <<- y                              #storing x in parent environment
    s <<- NULL                           #reseting data for new data
  }
  get <- function() x                    #returns matrix
  setmat <- function(inv) s <<- inv      #stores cached inverse
  getmat <- function() s                 #returns inverse matrix
  list(set=set, get=get,
       setmat = setmat,
       getmat = getmat)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  s <- x$getmat()                        #checking if cached inverse is same
  if(!is.null(s)){
    message('getting cached inverse')
    return(s)                            #returns cached inverse
  }
  data <- x$get()
  s <- solve(data, ...)                  #computes inverse
  x$setmat(s)                            #stores in cache
  s
        ## Return a matrix that is the inverse of 'x'
}
