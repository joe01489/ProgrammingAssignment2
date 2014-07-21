#makeCacheMatrix() and cacheSolve() work together to return the inverse
#of an invertible matrix, but doing it in such a manner that the inverse
#can be stored in the "cache," saving valuable computation time when
#calling the inverse again.

#makeCacheMatrix converts the input matrix into a special list that
#is used in cacheSolve

makeCacheMatrix <- function(x = matrix()) { #input is a matrix
      m <- NULL #clears the value of m
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
            #set() stores the value of the matrix
      get <- function() x #get() returns the matrix x
      setinv <- function(dummyvar) m <<- dummyvar
            #setinv() stores the inverse into m
      getinv <- function() m #getinv() returns the inverse m
      list(set = set, get = get, setinv = setinv, getinv = getinv)
            #returns a list of the functions and their names
}

#cacheSolve calculates the inverse if it has not been calculated already

cacheSolve <- function(x, ...) {
      #x is a list returned by the makeCacheMatrix() function
      m <- x$getinv() #uses the getinv() function to retrieve the inverse
      if(!is.null(m)) { #i.e. if m already has a value
            message("getting cached data")
            return(m)
      }
      data <- x$get() #uses the get() function to retrieve the matrix x
      m <- solve(data, ...) #solving for the inverse of the matrix x
      x$setinv(m) #uses the setinv() function to store m in the cache
      m #returns m
}