## This is homework exercise for Coursera course 
## "Data Science: Foundations using R" by Johns Hopkins University
## URL: https://www.coursera.org/learn/r-programming/home/welcome

makeCacheMatrix <- function(x = matrix()) {
  ## Create object with caching functional
  
  inversed_matrix <- NULL
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inversed_matrix <<- inverse
  get_inverse <- function() inversed_matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  ## Calculate an inverse of matrix 'x'
  ## Return the inverse from cache if it was stored there
  
  matrix <- x$get_inverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  inversed_matrix <- solve(data)
  x$set_inverse(inversed_matrix)
  inversed_matrix
}
