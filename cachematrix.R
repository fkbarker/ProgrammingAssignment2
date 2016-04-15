## These functions attempt to put inverse of a matrix in the cache for easy retrieval 


## makeCacheMatrix outputs a list of functions that: set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-solve
    getinverse <- function() m
    list(set=set,get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks to see if there is an inverse in the cache then gets it  
## if there is if not solve() the data which it is passed from the list. 
## Then stores the inverse in the cache for next time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
