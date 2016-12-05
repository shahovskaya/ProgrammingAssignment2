## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## By Kasya Shahovskaya, Dec 2016

## Matrix inversion is usually a costly computation 
## there may be a benefit to caching the inverse of a matrix rather than compute it.

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix function creates a list containing a function to
## a) set the value of the matrix -> b) get the value of the matrix -> 
## -> c) set the value of inverse of the matrix -> d). get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) z <<- inverse
    getinverse <- function() z
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Making inverse of the matrix. 
## Function checks if the inverse has already been computed: 
## TRUE: gets the result and skips the computation;
## FALSE: computes the inverse, sets the value in the cache via.

cacheSolve <- function(x, ...) {
    z <- x$getinverse()
    if(!is.null(z)) {
        message("receiveing cached")
        return(z)
    }
    data <- x$get()
    z <- solve(data)
    x$setinverse(z)
    z
}

