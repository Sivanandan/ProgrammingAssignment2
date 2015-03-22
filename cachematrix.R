
# In R; functions are technically called "Closures" containing its arguments; the main body and also the environment
# Here x is an invertible matrix; which is the input to "makeCacheMatrix"
# The function "makeCacheMatrix" returns a list containing functions that:
# A)Sets the matrix; B) Gets the Matrix; C) Sets the matrix's inverse D) Gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set_matrix = function(y){
        x <<- y
        inv <<- NULL   # Note that we use the "<<-" operator to assign values to objects in a different environment than function(y)'s environment
        }
    get_matrix = function() x
    set_inv = function(inverse) inv <<- inverse
    get_inv = function() inv
    list(set_matrix=set_matrix, get_matrix=get_matrix, set_inv=set_inv, get_inv=get_inv)
}
# Note that function above "makeCacheMatrix" otputs a list of functions

############################################################################

## The next function ("cacheSolve) takes the list of functions outputted by ("makeCacheMatrix") and returns the "inverse of the original matrix". However it also checks to see if the inverse has been calculated (using '!is.null (inv)' in conjunction with an if-else loop)
cacheSolve <- function(x, ...) {
    
       inv = x$get_inv()
       ## Now check and see if inverse is already known
       if (!is.null(inv)){
           message ("getting inverse from cache")
           return(inv)
        }
       
       ## If inverse is not known; calculate the inverse and set the value of inv using 'set_inv' function
       matrix_data = x$get_matrix()
       inv = solve(matrix_data)
       x$set_inv(inv)
       return (inv)
       
}
