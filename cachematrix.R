## Put comments here that give an overall description of what your
## functions do
# -------------------------------------------------------------------------
# The first function makeCacheMatrix creates a vector with 4 functions,   |
# the first one of which sets the values for the matrix and creates one,  |
# the second one gets the created matrix, the third one sets the inverted |
# matrix which was created in the function, and the fourth one gets the   |
# created inverted matrix.                                                |
# --------------------------------------------------------------------------
## Write a short comment describing this function                         |
## As mentioned before this creats a vector with the functions dealing    |
## with inputting and getting the matrix values.                          |
# --------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (matrix) {
    #sets the matrix values to x when the set function is invoked
    x <<- matrix
    #resets the inverse value
    inv <<- NULL
  }
  #gets the matrix values
  get <- function() x
  #sets the inverse values into the cache if it already exists
  setinv <- function(inverse) inv <<- inverse
  #gets the inverse values from the ones stored in the cache
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# ---------------------------------------------------------------
# Write a short comment describing this function                |
# This function solves for the inverse of the matrix            |
# that was created in the previous step. If there is already    |
# a solution present in the cache then the function will just   |
# return the inverse values. The arguments is the previously    |
# created list with 4 functions.                                |
# ---------------------------------------------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # Checks for presence of the cache, if present gives a message.
  if (!is.null(inv)) {
    print("Searching the cache, please wait just a second")
    return(inv)
  }
  #gets the matrix data to calculate the inverse
  matrix_data <- x$get()
  #solves for the inverse
  inv <- solve(matrix_data, ...)
  #print(inv)
  #sets into the cache
  x$setinv(inv)
  inv
}

#-----Sample use of the functions and demonstration-----         
sample <- makeCacheMatrix()                             
#Creating a square matrix and setting the values in
sample_matrix <- matrix (16:19, 2, 2)
sample$set(sample_matrix)
#Solving for the inverse
cacheSolve(sample)
#-------------------------------------------------------

