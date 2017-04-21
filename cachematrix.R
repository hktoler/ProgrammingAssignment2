## Programming Assignment 2: Lexical Scoping - by Haim Kotler 21 April 2017

## makeCacheMatrix is wrapping the matrix x and storing the cached value of its inverse
## in a variable called 'inv', defined within the scope of the function.
## 'inv' is set the first time the inverse is calculted or after matrix was cahnged
## and can be fetched later from cache without doing the calculation again.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvert <- function(i) inv <<- i
  getinvert <- function() inv
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## cacheSolve is used to get the inverse of a matrix the was previously 
## wrapped by makeCacheMatrix. 

cacheSolve <- function(x) {
    inv <- x$getinvert()
    if(!is.null(inv)) return(inv)
    data <- x$get()
    inv <- solve(data)
    x$setinvert(inv)
    inv
}
