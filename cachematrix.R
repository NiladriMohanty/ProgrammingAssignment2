## It computes the inverse of a square matrix
## But it uses caching method
## That means if elements of a matrix are not chaging,
## then it is not wise enough to compute inverse repeatedly (e.g. in a loop)
## Because it is waste of resources and time
## By taking the advantage of lexical scooping of R programming language,
## the value of the inverse of a matrix is cached



## This function used to create a special matrix object that can cache its inverse
## This function contains a list of functions calling to set the value of the matrix,
## to get the value of the matrix, to set the value of the matrix inverse and
## to get the value of the matrix inverse.

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




## This function calculates the inverse of a matrix created by the above function
## First, it checks matrix inverse has already been calculated
## If so, it retrieves the inverse from the cache and skips the computation
## If matrix inverse is not calculated, 
## it calculates the inverse of the matrix by solve(X) function
## and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting inverse of data")
                return(inv)
        }
        data <- x$get()
	inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

