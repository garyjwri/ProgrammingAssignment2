## The following pair of functions enable the inverse of a matrix
## to be cached.

## This function creates a "matrix" object that is a list of four
## functions.
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse<- function(inverse) inv<<-inverse
    getinverse<- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, getinverse=getinverse)
}

## This function will return the inverse stored in the matrix object.
## If the inverse has not been calculated, it calculates the inverse,
## and stores it in the cache.
cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix)
    x$setinverse(inv)
    inv
}
