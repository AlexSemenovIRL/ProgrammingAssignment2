## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function contains a list of functions, those are:
# set() - function that sets the initial parameters
# get() - function that gets the parameters that have been set, if any
# setsolve() - which solves the inverse of the matrix and assigns to m uisng the '<<-' (global assignement operator)
# getsolve() - simply gets the already computed inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
# cacheSolve() function will check if there are any computations already exist for the given matrix
# this is the main function that can be re-executed repeatedly and will give the cached putput where applicable

# First the x$getsolve() function will be invoked and if 'm' already been computed it will return
# the previous computation, which in our case is the inverse

# otherwise it will get the data again and compute the inverse and then finally save it into global space
# using the x$setsolve() function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
