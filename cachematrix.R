## Put comments here that give an overall description of what your
## functions do

## Function creates a matrix and caches the inverse

makeCacheMatrix <- function( x = matrix() ) {
  inv <- NULL ##null inverse
  set <- function( matrix ) { ##set matrix
    x <<- matrix
    inv <<- NULL
  }
  get <- function() { ##get matrix
    x ##matrix returned
  }
  setIt <- function(inverse) { ##set inv matrix
    inv <<- inverse
  }
  getIt <- function() {  ##get inv matrix
    inv
  }
  list(set = set, get = get, ## Return list
       setIt = setIt,
       getIt = getIt)
}

## This function calculates the inverse of a matrix first made by makeCacheMatrix

cacheSolve <- function(x, ...) {
  mtx <- x$getIt() ##return inverse of mtx
  if( !is.null(mtx) ) { ##catch error or previously set matrix
    return(mtx)
  }
  elem <- x$get() ## retrieve elements and calculate inverse
  mtx <- solve(elem, ...)
  x$setIt(mtx)
  mtx ##return inverse
}