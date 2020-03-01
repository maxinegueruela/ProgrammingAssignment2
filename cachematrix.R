## This programn calculates the inverse of a matrix and stores
#its value in the cache in order to consume less memory if I want
#to calculate the inverse of the same matrix again

#makeCacheMatrix: This function creates a special "matrix" object 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  get <- function() x
  setinvsematrix <- function(solve) m <<- solve
  getinvematrix <- function() m
  list(set = set, get = get,
       setinvsematrix = setinvsematrix,
       getinvematrix = getinvematrix)
  
  
}



# cacheSolve: This function calculates the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed and is square), then the cachesolve retrieves the inverse 
cacheSolve <- function(x, ...) {
        
  m <- x$getinvematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #solve() calculates the inverse of a square matrix
  x$setinvsematrix(m)
  m
  
  }
