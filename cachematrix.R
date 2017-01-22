## The central idea is to avoid long calculations when they are not needed. In this case, we are 
## avoiding the calculationg of the inverse of a matrix

## The first function makeCacheMatrix creates a list of "subFunctions" to 
## - get and set the original matrix to be inversed and
## - get and set the inversed matrix 

## it is interesting to notice that the function does not calculate the matrix inverse. It just stores the matrices (original and inversed)
## with the special operator <<- into another environment. That environment is not the environment (functions) where they are defined, hence, they
## can be stored and recovered again.

## It is interesting to notice that the set "subfunction" defines the stored inversed matrix as null once tha original matrix has changed.

makeCacheMatrix <- function(x = matrix()) {
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


## The cacheSolve function uses the makeCacheMatrix to set and get the a matrix and its inverse. It always try to use a cached inversed
## matrix to reduce computing time. If inv = NULL than there is no cache and it is necessary to calculate and store a new inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


## in order to test the above functions you can uncommnet the following lines, run the code and see the caching in action.

#### Testing the function
#B = matrix(c(1,3,2,4,5,6,7,8,9), nrow=3, ncol=3)
#C = solve(B)
#print(C)
#matx = makeCacheMatrix(B)
#resx = cacheSolve(matx)
#print(resx)
#resx = cacheSolve(matx)
#print(resx)
#resx = cacheSolve(matx)
#print(resx)
