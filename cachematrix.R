## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix() creates a special matrix that can cache its invers. This function has 4 sub functions: get, set, getInverse,setInvers
# 1. get function - Returns the matrix x stored in the main function
# 2. set function - Changes the matrix stored in the main function
# 3. getInverse & setInverse functions - Similar to get & set functions but they don't inverse but simply store/get the inverse values in the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) 
{
      m <- NULL
      set <- function(y) {
           x <<- y
          m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## Write a short comment describing this function

## Function cacheSolve() creates a special matrix that has inverse of a matrix. 
# This function gets the  inverse matrix in m using the function x$getInverse() and if it is already cached data then it will return m value.
#If there is no cached value, then it will use the function x$get() to get the matrix data. It will use the solve(data,..) function to inverse the matrix and 
# store it in cache using the x$setInverse() function. Finally, the inverse matrix is returned by m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  # If m is not null or there is cached data then return m or else proceed to next x$get
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
