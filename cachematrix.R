# The makeCacheMatrix function creates the matrix object that can cache its 
# inverse
#
# it contains one argument:
# x - this a numerical matrix which is invertible
#
# it contains four functions:
# * set           - to set the value of the matrix
# * get           - to get the value of the matrix
# * setInverse    - to set the inverse matrix
# * getInverse    - to get the inverse matrix
#
# The makeCacheMatrix retruns a list of the four internally specified functions.
# Or in other words the methods (set, get, setInverse, getInverse) applicable  
# on the object as defined by the function
# 

makeCacheMatrix <- function(x = matrix()) { 
      
      inv <- NULL       #inv is the cache matrix, which is set to be NULL to
      # start with.So it resets to NULL everytime
      #  makeCacheMatrix is called
      
      # the set function is used to store the matrix
      set <- function(y) { 
            x <<- y # a new value is assigned to x
            inv <<- NULL #so we flush the cache
      }
      
      #the get function is used to return the matrix of x which can be used
      # for computation
      get <- function() x 
      
      #This function is used to cache the value
      setInverse <- function(inverse) inv <<- inverse #storing the inverse
      #in the cache variable
      
      # This function returns the cache value
      getInverse <- function() inv        # returning the cache variable
      
      #the list of returned elements, which are the functions
      #in this way we can access the function from elsewhere 
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

# The cacheSolve function uses the "solve" function to get an inverse of the
# cached matrix from makeCacheMatrix. When calculated it stores this matrix 
# for later use. If the inverse has already been computed, it will skips the 
# computation and give you the matrix. 
#
# This function calls upon the functions of the makeCacheMatrix function. 
#
# The function returns the inversed matrix


cacheSolve <- function(x, ...) {
      
      inv <- x$getInverse() # getting the cache as speficied in makeCacheMatrix
      
      #using the if loop to return value from cache
      if(!is.null(inv)) { #this sentence examines whether inv (our cache) is NULL
            #or whether it contains a value - note that inv resets 
            #to NULL every time makeCacheMatrix is called  
            #When !is.null - in other words there is a value stored
            #in inv - it will return this stored value 
            message("getting cached data") # the message to tell you the value
            # was stored in the cache
            return(inv)   #returning the value from cache
      }
      #when no value is in the cache
      data <- x$get()     #taking the matrix out of the cachable matrix
      inv <- solve(data, ...) # calculating the inverse
      x$setInverse(inv) #the inversed matrix is stored in the cachable matrix
      
      #returning/printing the inverse cached matrix
      inv
}
