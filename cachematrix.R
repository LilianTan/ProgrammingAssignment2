## Put comments here that give an overall description of what your
## functions do

### Coursera Course: R Programming
#Week 3 programming Assignment
#Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
             x<<-y
             inv <<- NULL
          }
          
          get <- function() x
          set_inverse <- function(inverse) inv <<- inverse
          get_inverse <- function() inv
          list(set=set, get=get,
               set_inverse = set_inverse,
               get_inverset = get_inverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$set_inverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
