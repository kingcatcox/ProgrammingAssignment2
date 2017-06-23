## These functions create special matrix objects that contain a cached
## copy of their inverse to save sompute time when it is used multiple times

## Special contructor containing internal functions that manipulate the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    s <- NULL
    set <- function(y)
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    getsolve <- function() s
    setsolve <- function(solve) s<<-solve
    list( set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## function to store the solution to solve when calculated and present it in subsequent gets

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  s<-x$getsolve()
  if (!is.null(s))
  {
    message("Getting cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data)
  x$setsolve(s)
  s
}
