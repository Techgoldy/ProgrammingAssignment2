# As shown below these two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# => set the value of the matrix using set
# => get the value of the matrix using get
# => set the value of inverse of the matrix using setinverse
# => get the value of inverse of the matrix using getinverse
makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Following function returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data.")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver

}


#TEST RUN
#> source("cachematrix.R")
#> x = rbind(c(1, -1/2), c(-1/2, 1))
#> m = makeCacheMatrix(x)
#> m$get()
#     [,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0
#> cacheSolve(m)
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#> cacheSolve(m)
#getting cached data.
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#> cacheSolve(m)
#getting cached data.
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#> 

