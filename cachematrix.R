##
## Caching the Inverse of a Matrix
## 

## Example operation of these functions
## MatrixForCache <- makeCacheMatrix(Matrix) - creates special matrix
## cacheSolve <- MatrixForCache - call this to get the inverse 
## calculates inverse the first time then retrieves from cache thereafter

## Call this function to create the matrix for inversion
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  cachedmatrix <- NULL
  
  ## Set the matrix in the environment
  set <- function(y) {
    
    x <<- y
    
    cachedmatrix <<- NULL
  }
  
  ## Get the original matrix
  get <- function() x
  
  ## Create the inverse of the matrix
  setinverse <- function(solve) cachedmatrix <<- solve
  
  ## Get the inverse of the matrix
  getinverse <- function() cachedmatrix
  
  ## return the functions in a list to providing access when called from other functions
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)  
}


## Call this function to return the inverse of the matrix either
## This function computes the inverse of the special "matrix" returned
## by makeCachMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function retrieves the inverse from the cache.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        ## y is the list returned from makeCacheMatrix 
        ## y associates x with makeCacheMatrix
    
  ## get the inverse of the matrix
  cachedmatrix <- y$getinverse()
  
  ## if matrix inverse is already calculated then get cached inverse matrix
  if(!is.null(cachedmatrix)) {
    
    message("getting matrix from cache")
    
    return(cachedmatrix)
  }
  
  
  ## if matrix is not cached then get data
  data <- y$get() 
  
  ## cache the inverse of the matrix
  message("caching inverse of matrix")
    
  cachedmatrix <- solve(data)
  
  y$setinverse(cachedmatrix)
  
  ## return cached matrix
  message("returning cached matrix")
  return(cachedmatrix)
  
}

