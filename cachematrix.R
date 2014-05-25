#The following functions create an object that stores a matrix and caches its 
#inverse


#the first function 'makeCacheMatrix' is a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix<-function(x=matrix()) {
        
        m <- NULL
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


#The second function 'cacheSolve' calculates the inverse of the matrix created 
#above afted checking to see if the inverse has already been calculated.  If so,
#it uses 'get' to get the mean from the cache.  Otherwise it calculates the 
#inverse of the matrix using 'setinverse'.

cacheSolve<-function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                
                message ("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m #returns the inverse of the matrix 'x'
}
