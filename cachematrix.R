## The two functions makeCacheMatrix,cacheSolve are used to store
## the results of assigned data or calculated data, whenever the user 
## need just simply call these functions(get,getInverse), will return the results. 
## No need to recalcute the data again.
## we can also set the matrix to that variable by calling the variable$set(matrix())


## makeCacheMatrix is used to set and get the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL

    ## reset the matrix cache when set to a new matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverse) m <<- inverse
    getInverseMatrix <- function() m
    list(set = set, get = get, setInverseMatrixe = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## cacheSolve checks if the matrix inverse is exist
## if it is exist then return the result
## otherwise it will calculate the invese and cache finally return the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix() ## assign getInverse to 'm'
    if(!is.null(m)) { ## if it is not null no need to calculate
        message("getting cached data")
        return(m)
    }
    data <- x$get() 
    m <- solve(data) ## if it is null calculate inverse
    x$setInverseMatrix(m)
    m
}

## Example:-
## Vmatrix<-makeCacheMatrix(matrix(c(1,2,3,4,5,6),nrow=2,ncol=2,byrow=TRUE))

## Vmatrix$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## cacheSolve(Vmatrix)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5


## Vmatrix$getInverseMatrix()
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## if you want do for another matrix simply set the matix by below
## Vmatrix$set(matrix(c(3,2,1,5,2,1),nrow=2,ncol=2,byrow=TRUE))
## cacheSolve(Vmatrix)
##           [,1]       [,2]
## [1,]  0.38461538 -0.1538462
## [2,] -0.07692308  0.2307692
