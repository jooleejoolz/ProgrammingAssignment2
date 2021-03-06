## makeCacheMatrix() - an invertible matrix function
## cacheSolve() - calculates the inverse of results returned by makeCacheMatrix 
## providing the results haven't already been calculated. If the matrix remains unchanged,
## function returns the cache.

## Invertable matrix function
makeCacheMatrix <- function(x = matrix()) 
{
  inverseOf <- NULL
  set <- function(y) 
  {
    x <<- y
    inverseOf <<- NULL
  }
  get <- function() x
  setinverseOf <- function(inverse) inverseOf <<- inverse
  getinverseOf <- function() inverseOf
  list(
        set = set, get = get,
        setinverseOf = setinverseOf,
        getinverseOf = getinverseOf
      )  

}


## 

cacheSolve <- function(x, ...) 
## x: makeCacheMatrix output

{
## Return a matrix that is the inverse of 'x'
  inverseOf <- x$getinverseOf()
  if(!is.null(inverseOf))  ## if inversOf is not null then get the cached data
  {
    message("getting cached data")
    return(inverseOf)
  }
  
  ## else calcs the inverse
  data <- x$get()
  inverseOf <-  solve(data, ...)
  x$setinverseOf(inverseOf)

  ## Return inverse of makeCacheMatrix
  inverseOf
}



