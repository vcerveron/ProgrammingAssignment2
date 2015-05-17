## compute and cache the inverse matrix
## by using lexical scoping

## create a method for the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## get or compute the cache inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse 
}


## function to test the assignment

testAssignment2 <-function() {
  m1 <- matrix(nrow=2, ncol=2)
  m1[1,1] <- 4
  m1[1,2] <- 0
  m1[2,1] <- 0
  m1[2,2] <- 20
  m <- makeCacheMatrix(m1)
  print("showing contents of the special matrix")
  print(m$get()) 
  minv <- cacheSolve(m)
  print("showing inverse matrix")
  print(minv) ## showing the inverse matrix
  print("compute product to identity matrix")
  print(m1*minv)
  
  minv2 <- cacheSolve(m)
  print("showing inverse matrix again")
  print(minv2) ## showing the cache inverse matrix
  print("compute product to identity matrix")
  m$get()*minv2
}
