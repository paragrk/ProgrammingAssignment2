## These functions cache matrix inverse so that next time if the same matrix is passed then 
# inverse is not calculated again instead returned from cache. 

## Creates an empty matrix to store inverse. Also creates functions to get input matrix, get inverse and to set inverse

makeCacheMatrix <- function(x = matrix()) {
  minverse <- matrix()
  get <- function() x
  setinverse <- function(invr) minverse <<- invr
  getinverse <- function() minverse
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks if the matrix inverse has already been calculated. 
## If it has been then returns inverse from cache otherwise calulates inverse using Solve
## Assumption here is that the matrix is square and invertable

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.na(minverse[1,1])) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data)
  x$setinverse(minverse)
  minverse
  
}
