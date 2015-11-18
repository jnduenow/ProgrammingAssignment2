## Creates a list of functions that are used to manipulate matrices
## set(input_matrix):  sets the value of a matrix to the value provided in argument,
##   and sets inverse matrix value to NULL
## get():  returns the value of a matrix
## setinverse(input_matrix):  sets the value of a matrix to the value provided in argument
## getinverse():  returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     
     ##set initial value of inverse matrix to NULL
     inverseMatrix <- NULL
     
     ##set function:  sets value of matrix 'x' to value of matrix 'y' provided in argument
     set <- function(y) {
          x <<- y
          inverseMatrix <<- NULL
     }
     
     ##get function: returns value of matrix 'x'
     get <- function() x
     
     ##setinverse function:  applies solve function and stores value to inverseMatrix
     setinverse <- function(solve) inverseMatrix <<- solve
     
     ##returns value of inverse matrix 'inverseMatrix'
     getinverse <- function() inverseMatrix
     
     ##creates list of matrix manipulation functions available
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Return a matrix (invertedMatrix) that is the inverse of input matrix 'x';
## the input to this function should be the output of the function above.
## If the inverse matrix previously exists, it reads the value of the inverse
## matrix from memory.  If the inverse matrix did not previously exist, the
##function computes the value of the inverse matrix and stores it for future use.

cacheSolve <- function(x, ...) {
     
     ##get the value of the inverse of x, if it exists (may be NULL)
     invertedMatrix <- x$getinverse()
     
     ##if the inverted matrix variable is not null, load value from memory rather than calc.
     if(!is.null(invertedMatrix)) {
          
          ##send a message to the console indicating that value will be loaded
          message("getting cached inverse matrix")
          
          ##return the value of the inverted matrix, exiting function
          return(invertedMatrix)
     }
     
     ##get (load) the input matrix, x, into variable inputMatrix
     inputMatrix <- x$get()
     
     ##calculate the inverse of the input matrix and store in variable invertedMatrix
     invertedMatrix <- solve(inputMatrix, ...)
     
     ##save the value of the inverted matrix using setinverse() function
     x$setinverse(invertedMatrix)
     
     ##return the inverted matrix
     invertedMatrix
}
