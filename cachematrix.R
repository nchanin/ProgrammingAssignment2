## Noah Chanin
## 22-MAY-2014

## makeCacheMatrix takes in a matrix
## the matrix is assumed to be a square invertible matrix
## it can return the matrix via getdte()
## or save the answer to the cache via setcache
## or return the answer via getcache

makeCacheMatrix <- function(x = matrix()) {
  answer <- NULL
  
  ## y 
  set <- function(y) {
    x <<- y
    answer <<- NULL
  }
  get <- function() x
  setcache <- function(cache) answer <<- cache
  getcache <- function() answer
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## cacheSolve takes an instance of makeCacheMatrix as its input
## if this instance has already been cached, the cached value is returned
## otherwise the answer is generated
## and cached via setcache
## and the answer is returned to the caller

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedval <- x$getcache()
  if(!is.null(cachedval)) {
    message("getting cached data")
    return(cachedval)
  }
  ## otherwise...
  data <- x$get()
  answer <- solve(data, ...)
  x$setcache(answer)
  answer
}


## testdata
# from ?solve
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8); h8
# sh8 <- solve(h8); sh8
# source("cachematrix.R")
# test <- makeCacheMatrix(h8)
# cacheSolve(test)
## expect first run to not say cached, and results to match sh8
# cacheSolve(test)
## expect second run to say cached, and return prior results

