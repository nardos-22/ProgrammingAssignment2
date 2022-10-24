## To write a pair of functions called "makeCacheMatrix" and "makeCacheMatrix"
## makeCacheMatric is a function which creates a special "matrix" object that can cache its inverse for the input

## makeCacheMatrix consists of set, get, setInv, and getInv
makeCacheMatrix <- function(x = matrix()) {
        inv <- Null               ## initiatializing Inverse as Null
        set <- function(y){
                x <<- y
                inv <<- Null
        }
        get <- function() x           ## function to get matrix x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() {
                                  inverse <- ginv(x)
                                  inver%*%x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = get Inverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {                     ## checking whether inverse is Null
                message("getting cached data!")
                return(inv)
        }  
        mat <- x$get()
        inv <- solve(mat, ...)             ## calculates inverse value
        x$setInverse(inv)
        inv
}                                     ## return a matrix that is inverse of 'x'
