#Because of calculation of the inverse of a matrix its very costly in 
#computational terms, we want to create a pair of function whose objective is	
#to avoid redundant calculations. 
#We will save the inverse matrix of the given one in cache and then recover 
#it instead of repeat all calculus.


#makeMatrix builds a set of functions and returns the functions within a list
#to the parent environment.
#The function creates a special "matrix" object that can cache its inverse.


makeMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cachesolve will calculate the inverse matrix of the given one only if 
#the data wasn't in the memory. In other case, it return the result of the previous
#calculation. 


cachesolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("here is pre-calculated data!")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinverse(inv)
        inv
}