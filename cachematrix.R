# The first two functions in this document create a cache system for the inverse of a matrix. The two functions below are the examples from the assignment, with a bit of an explanation.

# The function below takes a matrix as an input, and caches the value itself, and the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        SetMatrix <- function(y){
                x <<- y
                inverse <<- Null
        }
        getMatrix <- function() x 
        setInverse <- function(solve) inverse <<- solve 
        getInverse <- function() inverse
        list(setMatrix = setMatrix,
                getMatrix = getMatrix,
                setInverse = setInverse,
                getInverse = getInverse)
}


# The function below takes the above function as an argument, which means it deals with the cached values of them. It will return the inverse if it has already been calculated, and calculates it if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()                                # Sets inverse to whatever value is returned when it calls the getInverse() function (so whatever Inverse is)
        if(!is.null(inverse)) {                                  # If inverse is not NULL,
                message("getting cached data")                   # It will print this message
                return(inverse)                                  # And returns the value of inverse
        }
        data <- x$getMatrix()
        inverse <- mean(data, ...)
        x$setInverse(inverse)
        inverse
}

# Here are the sample functions from the assignment, with explanations as to what they do:

makeVector <- function(x = numeric()) {
        m <- NULL                                       # Sets new variable "m" to NULL
        set <- function(y) {                            # Creates a function that takes "y",   
                x <<- y                                 # assigns it to x in the global environment,
                m <<- NULL                              # makes sure m is NULL
        }
        get <- function() x                             # Creates a function that returns x from set
        setmean <- function(mean) m <<- mean            # Creates a function that sets m to mean (not fully sure what this does)
        getmean <- function() m                         # Creates a function that returns m
        list(set = set, get = get,                      # Creates a list of the four functions
             setmean = setmean,
             getmean = getmean)
}

# The following function calculates the mean of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function

cachemean <- function(x, ...) {                         # Creates function with input x, which is the list from the other function
        m <- x$getmean()                                # Sets m to whatever value is returned when it calls the getmean() funciton (so whatever m is)
        if(!is.null(m)) {                               # If m is not NULL,
                message("getting cached data")          # It will print this message
                return(m)                               # And returns the value of m
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}