## R Programming - Coursera/Data Science Track
## Jeremy Short - Sunday Feb. 22, 2015
## Programming Assignment 2: Lexical Scoping

## Function 1: the makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL                                         #Clear out s
      set <- function(y) {                              #Clear out any exisiting cache
            x <<- y
            s <<- NULL
      }
      get <- function() x                               # get() returns the matrix
      setsolve <- function(solve) s <<- solve           # creates the inverse matrix
      getsolve <- function() s                          # returns the inverse matrix
      list(set = set, get = get,                        # creates a list of all variables in function
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()                                 #Runs & assignes the solve() in previous functions
      if(!is.null(s)) {                                 #Checks if cache exists. If there is a cache it uses it.
            message("getting cached data")              #Lets Users know that data is coming from cache
            return(s)
      }
      data <- x$get()                                   # Gets the inverse of the matrix
      s <- solve(data, ...)
      x$setsolve(s)
      s                                                 #returns the inverse
}
