## combined set of functions aimed at calculating the inverse of
## a non-singular matrix and outputting it efficiently avoiding
## recalculations if there is no change in the original matrix and
## its inverse has already been calculated

## Function to cache matrix content

makeCacheMatrix <- function(x = matrix()) {
      # initialize inverse matrix
      m <- NULL
      # Store matrix data from external values
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      # get previously stored matrix data
      get <- function() x
      #calculate the inverse of a matrix using the solve function and store it
      set_inv <- function(solve) m <<- solve
      #get the inverve of a matrix previously stored
      get_inv <- function() m
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}

## Function to return the inverse of a matrix taking as input
## a special list (output of function makeCacheMatrix)

cacheSolve <- function(x, ...) {
      #getting the inverse matrix from stored data
      m <- x$get_inv()
      #checking if inverse matrix has already been stored and if matrix as changed
      if(!is.null(m)) {
            #checking if matrix as changed
            #if (solve(x$get())==x$get_inv()) {
            # output retrieve cached inverse matrix
            message("getting cached data")
            return(m)
      }
      #}
      # calculate inverse matrix
      message("calculating inverse of matrix")
      # store original matrix data in a temp variable
      data <- x$get()
      # calculate inversa matrix from temp variable
      m <- solve(data)
      # store calculated inverse matrix in reference list
      x$set_inv(m)
      # output calculated inverse matrix
      m
}
