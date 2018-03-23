## Put comments here that give an overall description of what your
## functions do

## Make cachable matrix object
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(im) {
    x <<- im
    inverseMatrix <<- NULL
  }
  get <- function(){
    return(x)
  }
  setInverse <- function(im){
    inverseMatrix <<- im
  }
  getInverse <- function(){
    return(inverseMatrix)
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse.matrix <- x$getInverse()
  if(!is.null(inverse.matrix)){
    message('getting cached inverse matrix')
    return(inverse.matrix)
  }
  dataMatrix <- x$get()
  inverse.matrix <- solve(dataMatrix)
  x$setInverse(inverse.matrix)
  return(inverse.matrix)
}


## EXTRA Function : Teste de functions
runTestCacheSolve <- function(testeMatrix = rbind(c(3,0, 2), c(9,1,7), c(1,0,1))){
  message("Matrix:")
  t1 <- system.time({
    a <- makeCacheMatrix(testeMatrix)
    print(a$get())
  })
  print(t1)
  
  message("Inverse Matrix:")
  t2 <- system.time({
    a_inverse <- cacheSolve(a)
    print(a_inverse)
  })
  print(t2)
  
  message("Inverse Matrix, new run:")
  t3 <- system.time({
    a_inverse_again <- cacheSolve(a)
    print(a_inverse_again)
  })
  print(t3)
  
}

## RUN examples
runTestCacheSolve()
runTestCacheSolve(matrix(rnorm(1:36),c(6,6)))
runTestCacheSolve(matrix(rnorm(1:10000),c(100,100)))
runTestCacheSolve(matrix(rnorm(1:1000000),c(1000,1000)))