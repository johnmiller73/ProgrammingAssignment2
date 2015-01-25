## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL  ##Needed to reset the function
       set <- function(y) {  ##I left the m, y and x the same as the example function
             x <<- y
             m <<- NULL
         }
       get <- function() x
       setinverse <- function(solve) m <<- solve  ##I changed name to setinverse and the called function to solve instead of mean. 
       getinverse <- function() m
       list(set = set, get = get,
                       setinverse = setinverse, ##I left these the same although updated the name
                       getinverse = getinverse)
   }


## Write a short comment describing this function 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {  ##looks for the returned matrix from the makeCacheMatrix function 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ##Will prouce the inverse if not already indentified
  x$setinverse(m) 
  m  ##displays the resulting matrix
}
