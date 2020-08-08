## I will use the methodology that we  learned from the example, when we compute the mean of a vector,
## the procedure is very similar. I have only changed in order to get what we are asked for.


## This function will store the matrix in the cache. Therefore, we should put in the console, for example,
## matrix_example <- makeCacheMatrix(matrix(1:4, 2, 2)). If we want to see the matrix, we write 
## matrix_example$get() so the console will show us the matrix that we have just created. Then, if we want to see the inverse,
## we will not be able to see it because we have not yet computed the inverse.

makeCacheMatrix <- function(a = matrix()) {
                      inversa <- NULL
                      
                      set <- function(b) {
                      a <<- b
                      inversa <<- NULL
                      }
                     
  get <- function() {a}
  setInversa <- function(inverse) {inversa <<- inverse}
  getInversa <- function() {inversa}
  list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}

## This function will help us to find the inverse of the matrix that we have just created with the function "makeCacheMatrix".
## In that sense, we just write in the console "cacheSolve(matrix_example)", then we have compute the inverse of that matrix. 
## If we have already compute the inverse, it will show a message that the data is already in the cache so the process of finding
## the inverse will not procedure. 

cacheSolve <- function(a, ...) {
                      inversa <- a$getInversa()
                      
                      if(!is.null(inversa)) {
                        message("Getting cached data")
                        return(inversa)
                      }

  mat <- a$get()
  inversa <- solve(mat, ...)
  a$setInversa(inversa)
  inversa
}
