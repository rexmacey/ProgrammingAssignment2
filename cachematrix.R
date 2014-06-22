## These functions return the inverse of a matrix.
## If the matrix has been inverted, a cached value is returned

## Sets, gets the matrix, sets and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x<<- y
        m<<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m<<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse=  getinverse)
}


## Inverts the matrix getting the result from the function above if is has alredady been calculated
## If not, it performs the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        #message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
