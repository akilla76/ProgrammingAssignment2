## Caching the Inverse of Matrix for imrpoving performance 


## This function stores matrix and its inverse
## arguments are functions
## $get returns stored matrix
## $set stores matrix
## $getInverseMatrix returns inversed matrix if it's not NULL
## $setInverseMatrix stores inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  get<-function() x
  set<-function(y) {
    x<<-y
    inv<<-NULL
  } 
  
  getInverseMatrix<-function() {
    if (!is.null(inv)) {
      message("cache")
      return(inv)
    }
  }
  setInverseMatrix<-function(inv1) inv<<-inv1
  
  list(get=get, set=set, getInverseMatrix=getInverseMatrix, setInverseMatrix=setInverseMatrix)
}


## This function calculates inverse of matrix if it's not calculated before
## if it's been calculated then return stored inversed matrix 
## Argument
## x function of makeCacheMatrix type
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}
