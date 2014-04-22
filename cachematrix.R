#############################################################################################
## This two functions makeCacheMatrix() and cacheSolve() allow
## the caching big matrix and its inverse matrix.
## It may take too long for a big matrix to compute its inverse,
## especially if it has to be computed repeatedly.
## It has sense to cache the inverse matrix rather than recompute it again.
#############################################################################################


## The makeCacheMatrix() function creates special matrix object for geeting
## and setting matrix and the inverted matrix.
## The "<<-" operator assigns a value to an object in an environment 
## that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
   mat <- NULL                                 ##initializing inverse matrix to NULL
   set <- function(y) {                        ##setting value of marix
     x <<- y
     mat <<- NULL
   }
   get <- function() x                         ##getting value of marix
   set.inv <- function(inv) mat <<- solve      ##setting value of the inverted marix in cache
   get.inv <- function() mat		       ##getting value of the inverted marix
   list(set = set, get = get,		       ##function returns list of functions
        set.inv = set.inv,
        get.inv = get.inv)
 }

#############################################################################################
## The cacheSolve() function returns inverted matrix
## First the function checks if the matrix was computed.
## If yes, the inverse matrix is getting from cache.
## If no, than it computes the inverse and set it in the cache.
 
 
 
cacheSolve <- function(x, ...) {             
         
   mat <- x$get.inv()			       ##getting inverted matrix from cache
   if(!is.null(mat)) {			       ##checking if the inv. matrix is already stored
     message("getting cached data")
     return(mat)			       ##if it is stored, it returns stored inv. matrix
  }
   data <- x$get()			       ##if it is not stored, getting original matrix
   mat <- solve(data, ...)		       ##inverting matrix
   x$set.inv(mat)			       ##setting inverted matrix to cache
   mat			                       ##return inverted matrix
 }
#############################################################################################

