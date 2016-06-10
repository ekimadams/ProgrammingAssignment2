## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The Two functions below cah be used to create a special object that stores a 
## matrix and caches the inverse.



# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Following the same format as the assignment example
  # Creating a makeCacheMatrix object will consist of
  # four functions encapsulated in a list
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Creates and returns a list
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinverse()
  

  if(!is.null(inv)) {
    # Return the cached computed inverse and print message to know it was cached		
    message("Retrieving the cached matrix")
    return(inv)
  }
  
  dat <- x$get()
  
  inv <- solve(dat, ...)

  x$setinverse(inv)
  inv    
}

