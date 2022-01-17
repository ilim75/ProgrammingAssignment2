## Below are two functions that are used to create a special object that stores
## a numeric matrix and cache it's inverse 

## This function creates a special "matrix" which is a list that:
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the inverse
## 4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function, but first checks if the inverse has already
## been calculated. If so, it gets the inverse from the cache and sets the matrix,
## otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
