## Emmanuele Bello
## this function is like a class to prevent expose variables outside 
## using <<- operator.


makeCacheMatrix <- function(x = matrix()) {
 #inverted matrix set to NULL
  invMatr <- NULL
  
  set <- function(y) {
    x <<- y
    invMatr <<- NULL # it also initialises invMatr to null
  }

   # get input matix
  get <- function() x

  # set & get invMatr
  setInv <- function(inv) invMatr <<- inv 
  getInv <- function() invMatr
  
  # list with basic functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mInv <- x$getInv()
        if(!is.null(mInv)) { 
        return(mInv) # return the calculated inversion because in result
  }
  
  data <- x$get() # get mattrix object
  
  # using solve to calculate the inverted matrix
  mInv <- solve(data) 
  
  # set calculated inverse matrix to our object
  x$setInv(mInv) 
  
  # we got it!
  mInv 
}
