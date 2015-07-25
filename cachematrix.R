## Returns the inverse of the matrix and caches it for later recall

## Create matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            # Changes matrix stored in main function
                x <<- y                                 # set 'x' as input ('y')
                m <<- NULL                              # restore NULL matrix 'm'
        }
        get <- function() x                             # return matrix stored in 'x'
        setsolve <- function(solve) m <<- solve         # store value of solved matrix 'm'
        getsolve <- function() m                        # return value of solved matrix 'm'
        list(set = set, get = get,                      # store function in a list
             setsolve = setsolve,
             getsolve = getsolve)
}


## Calls inverse of the matrix if it does not already exist

cacheSolve <- function(x, ...) {
        m <- x$getsolve()                               # set 'm' as solved matrix (if it exists)
        if(!is.null(m)) {                               # if m is NOT null, then return cached solution 
                message("getting cached data")
                return(m)
        }                                               # if m is null, then solve the matrix...
        data <- x$get()                                 # stores matrix 'x' in 'data'
        m <- solve(data, ...)                           # stores solution of 'data' in 'm'
        x$setsolve(m)                                   # caches the new solved matrix 
        m                                               # Prints the solved matrix
}
