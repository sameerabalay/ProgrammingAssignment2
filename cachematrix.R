## cachematrix.R files contains functions to return the inverse of a matrix 
## from the cache if it already calculated otherwise it calculates the inverse
## of the matrix and returns the inverse\## Review the following pdfs for 
## learning about inverse of a matrix. It pdfs have example data with the formula
## http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices7-2009-1.pdf
## http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices11-2009-1.pdf
## http://www.mathcentre.ac.uk/resources/Engineering%20maths%20first%20aid%20kit/latexsource%20and%20diagrams/5_5.pdf
## Usage 
##      1. Construct the matrix 
##              Ex: matrix2 <- matrix(seq(1:4), 2)
##      2. Create the cache of the matrix
##              Ex: z <- makeCacheMatrix(matrix2)
##      3. Call the cacheSolve of the matrix for getting the inverse 
##              Ex: cacheSolve(z)

## makeCacheMatrix function creates a cache of the matrix passed. 
## To use cacheSolve you have to use makeCacheMatrix first. It has functions
## which sets the matrix through set(), gets the matrix through get(), 
## setInverse() sets the local inverse variable 'inv' with the inverse passed.
## getInverse() gets the 'inv' variable
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ## reassign the new matrix passed. Since it is a new matrix 
                ## the inv variable should be null since it needs to be 
                ## recalculated.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns an inverse of a matrix from the cache if it is already
## calculated otherwise it generates it using the solve(). Sending a matrix to 
## the solve function returns an inverse of a matrix. 
## If determinant is zero then the inverse throws the following error message
## Error in solve.default(data) : 
## Lapack routine dgesv: system is exactly singular: U[2,2] = 0 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## inversematrix <- solve(x)
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
  
}


