## Here are two functions 'makeCacheMatrix' and 'cacheSolve'
## They allow calculating the inverse of an invertible matrix.
## If the inverse has not been calculated before, it is calculated anew
## and stored to cache. If it has already been calculated before it is 
## read from cache instead of repeating the calculation.

# The first function, `makeCacheMatrix` creates a special "vector", which is
# a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i   <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(invers) i <<- invers
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, 'cacheSolve' calculates the inverse of a matrix. 
## In case this inverse has been calculated before, the solution is simply
## read from cache. Otherwise it is newly calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}


### To test the code, uncomment the following:

# # Create an invertible matrix 
# mat <- matrix(rnorm(100), nrow=10, ncol=10) 
# 
# # Create functions with matrix and look at output
# cm <- makeCacheMatrix(mat)
# cm$get()
# cm$getinv()
# 
# # Calculate inverse
# cacheSolve(cm) # First call returns inverted matrix
# cacheSolve(cm) # Second call returns inverted matrix along with message that data comes from cache
