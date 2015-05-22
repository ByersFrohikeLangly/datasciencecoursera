##--------------------------------------------------
## Programming Assignment 2
## Written by Daniel Davis
## 05/22/2015
## cachematrix.R
##--------------------------------------------------

##--------------------------------------------------
## makeCacheMatrix
##--------------------------------------------------

## Function caculates the inverse of the matrix and
## creates a cached version for future use
makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

##---------------------------------------------------
## cacheSolve
##---------------------------------------------------

## Calculate the inverse of the matrix. If the inverse
## has already been caculated, uses the cached result.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
