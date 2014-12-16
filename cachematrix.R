## JL 16.12.2014 Coursera R Programming Assignment 2
## Optimization of inverting matrixes

## makeCacheMatric to create a vector for maintenance functions:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve to check if the inverted matrix already exists in the vector, if not, then solve() it to create the inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
        if(!is.null(m)) { # check the cached vector
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # calculate the inversion
        x$setmatrix(m)
        m
}
