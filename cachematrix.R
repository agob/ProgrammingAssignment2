# makeCacheMatrix is a function that returns a "special" list 
#containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverted matrix
#4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Let'store the cached inverse matrix
  m_inv <- NULL
  # With the following we set the matrix (as it was done with the vector) using the <<- operator
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x #Get the matrix
  setinverse <- function(inverse) m_inv <<- inverse #set the inverse using the <<- operator
  getinverse <- function() m_inv #get the inverse
  
  # Return the final matrix 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function to compute the inverse of the matrix. 
#If the inverse has already been calculated, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  
  m_inv <- x$getinv()
  
  if (!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv) # Return the inverse if already calculated
  }
  
  data <- x$get()
  m_inv <- solve(data, ...) #we calculate here the inverse using solve
  x$setinverse(m_inv) #caching the inverse
  m_inv
  
}
