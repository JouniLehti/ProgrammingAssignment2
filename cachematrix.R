## JL 16.12.2014 Coursera R Programming Assignment 2
## Optimization of inverting matrixes

## makeCacheMatric to create a vector for maintenance functions:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

## usage example:
## > solvablematrix <- makeCachedMatrix() 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcachedmatrix <- function(solve) m <<- solve
        getcachedmatrix <- function() m
        list(set = set, get = get,
             setcachedmatrix = setcachedmatrix,
             getcachedmatrix = getcachedmatrix)
}

## cacheSolve to check if the inverted matrix already exists in the vector, if not, then solve() it to create the inversion
## usage: first, create your matrix and assign it to 'solvablematrix' with > solvablematrix@set(matrix(...))
## Then run it through: cacheSolve(solvablematrix)

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
	      m <- x$getcachedmatrix()
        if(!is.null(m)) { # check the cached vector
                message("getting cached dat<a") # report to be displayed if cache is used
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # calculate the inversion
        x$setcachedmatrix(m) # store the inverted result into the cache
        m
}
