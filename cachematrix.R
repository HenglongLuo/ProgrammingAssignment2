## The two functions aim to produce the inverse of a matrix with taking advantage of 
## caching and thus avoiding repeated computation. The first function creates a list 
## containing a function to set a matrix, get a matrix, set the inverse of a matrix, 
## and get the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x) {
        m <- NULL
        set <- function(data){
                x<-matrix(data)
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        } else {
                data <- x$get()
                m <- solve(data) %*% data
                x$setinverse(m)
                m
        }
}
