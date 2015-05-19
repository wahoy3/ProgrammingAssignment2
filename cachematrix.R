## The following code contains two functions:
## 1) The first one defines an object which consists of a matrix, possibly
##    its inverse and 4 different functions to set or retrieve those matrices.
## 2) The second function uses the first definition in order to invert a matrix,
##    including a first check whether the matrix inversion has been performed
##    before. If not, the inverse is computed and stored. If yes, the "cached"
##    inverse is retrieved without new calculation.


##-----------------
## makeCacheMatrix
##-----------------
## defines an object consisting of a matrix, its inverse, and 4 routines to
## set and get those matrices. Since the functions are stored as closure jointly
## with their environment, the matrices are still available via the 4 defined
## functions. Objects defined by this function are used by "cacheSolve" in order
## to invert a matrix only if it has not been inverted before.
##
## Input argument:
##    x = matrix(): matrix x can be initialized when a new object is defined in
##                  function call. Alternatively, when function is called without
##                  arguments, x is a 1x1 matrix containing NA and can be defined
##                  later on using $set functionality of the object.
## Internal variable:
##    xinv: object to store inverse matrix of x. xinv is initialized to NULL and
##          reset to NULL everytime a new matrix x is defined via $set.
##
## Output:
##    Objects defined via "makeCacheMatrix" are a list of the four functions $get,
##    $set, $getinv, and $setinv. Those functions can be used to set and retrieve
##    both matrix x and its inverse which are still available as part of the 
##    closure of the four funtions.
##
## Caution: Function $setinv allows to assign anything to xinv. Correct
## logic (i.e. guarantee that xinv indeed contains inverse of x) is ensured
## by cacheSolve.
##-----------------
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
      x <<- y
      xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##-----------------
## cacheSolve
##-----------------
## Function which calculates inverse matrix of an object defined by
## previous function "makeCacheMatrix". Inverse is calculated by solve().
## In case, inverse matrix is already available, the result is retrieved
## without further calculation.
##
## Input arguments:
##    x: Object defined by makeCacheMatrix.
##    ...: List of potential arguments used by solve().
## Internal variable:
##    data: matrix which is assigned the matrix x via x$get.
##
## Output:
##    xinv: inverted matrix obtained by inverting "data" by solve(). Result is
##          then stored in object x via $setinv and returned by cacheSolve.
##-----------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
