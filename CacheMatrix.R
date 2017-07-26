## These functions will work in conjunction to:
## a) Create a matrix that has the ability to cache its inverse after I input it into 'makeCacheMatrix' function, and
## b) Solve the inverse of the created matrix by inputting into the 'cacheSolve' function, which first checks for 
##    previously solved matrices identical to the inverse of the matrix input, and either outputs the cached
##    answer or solves the matrix inverse, then caches for recall.
##    
##    This combination of functions can dramatically decrease hardware loads, and UI output response time to input.
##    

##    makeCacheMatrix provides the environment for the input of a matrix to have the ability for caching its inverse. 

makeCacheMatrix <- function(a = matrix()) {
        inv <- NULL
        set <- function(c) {
                a <<- c
                inv <<- NULL
        }
        get <- function() a
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##    cacheSolve function, solves the inverse then caches it; if the same matrix input is provided then the cached 
##    inverse is retreived.
##

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
        inv <- a$getInverse()
        if (!is.null(inv)){
                message("Retreiving Cached Data (RCD)")
                return(inv)
        }
        g <- a$get()
        inv <- solve(g, ...)
        a$setInverse(inv)
        inv
}
