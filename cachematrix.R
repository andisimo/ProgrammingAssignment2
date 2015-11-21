## This function receives a parameter x, which is a matrix, and returns
## a list of functions. 

makeCacheMatrix <- function(x = matrix()) 
{
     ## m is the variable to be used to store the inverse of the matrix
     m <- NULL
     
     ## the set() function caches the x variable in the environment
     ## where it is created. The value for m is set to NULL because
     ## when x is assigned a new value, the inverse, m, must change also. 
     ## y is passed when the function is called from the list that is
     ## returned at the end of the function. 
     set <- function(y)
     {
          x <<- y 
          m <<- NULL
     }
     
     ## get returns the matrix x
     get <- function() x
     
     ## setInverse() receives the parameter that is the inverse of x,
     ## then caches that value in the function's environment (where the
     ## variable is created)
     setInverse <- function(inverse)
     {
          m <<- inverse
     }
     
     ## getINverse returns the inverse
     getInverse <- function() m
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve receives a list of functions returned by makeCacheMatrix, and
## returns the inverse. It gets the inverse from the cache in the makeCacheMatrix
## environment if the cached variable has a value, otherwise it calculates the inverse
## and saves it to the cache.

cacheSolve <- function(x, ...) 
{
     ## get the inverse of the matrix from the getInverse() function in makeCacheMatrix
     m <- x$getInverse()
     
     ## if the value returned from x$getInverse() is NOT NULL, return the cached value.
     if(!is.null(m))
     {
          message("getting cached data ...")
          return(m)
     }
     
     ## if the value returned is NULL, compute the matrix, call setInverse() to cache it,
     ## and return it 
     inverseMat <- x$get()
     m <- solve(inverseMat)
     x$setInverse(m)
     
     return(m)
}
