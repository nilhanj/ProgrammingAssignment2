## These two functions allow you to create a special "matrix" object that caches its inverse.
## Then allow you to get the cached version of the inverse if it exists. Without having to 
## compute the inverse again. Which saves time and computing resources

## makeCacheMatrix takes in a matrix and creates a cache object and stores the inverse 
## of the matrix. 

makeCacheMatrix <- function(x = matrix()) { # takes a matrix object as argument  

  inverse <- NULL  # create an object to store the matrix and initialises it to NULL

  # Allows you to change the values of the matrix object from outside and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x # returns the matrix object
  setinverse <- function(solve) inverse <<- solve(x) # cache the inverse of the matrix 
  getinverse <- function() inverse # return the inverse of the matrix
 
  # creates the methods for manipulating the matrix object
  list(set =set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes, caches, and returns the inverse of the  matrix 

cacheSolve <- function(x, ...) { # takes a variable x as an argument
        
  inverse <- x$getinverse() # find inverse of matrix created by makeCacheMatrix
  
  # if inverse was not cahced i.e not NULL then return inverse
  if(!is.null(inverse)) {
    message("getting cached data") # message to say the cahced inverse is being returned
    return(inverse)
  }
  data <- x$get() # get the values of the matrix created by makeCacheMatrix 
  inverse <- solve(data, ...) # use solve to calculate inverse of matrix
  x$setinverse(inverse) # call the setmean function in makeCacheMatrix to set the new value
  
  inverse ## Return a matrix that is the inverse of 'x'
  
  
}
