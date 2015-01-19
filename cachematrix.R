## Following pair of functions create a special object that stores a matrix object and cache's inverse of the matrix
## If the contents of a matrix are not changing and are present in the cache, then the inverse of that matrix are 
## looked up in cache and returned as output. 
##If the matrix gets changed then its inverse is computed and stored in cache.

##To Test this pair of functions :->
#source("cachematrix.R")
#a1 <- matrix(data = c(1,2,2,1), nrow = 2, ncol = 2) -> a1 is matrix
#a2 <- makeCacheMatrix(a1)  -> a2 is list
#cacheSolve(a2) -> computes the inverse and returns as output
#cacheSolve(a2) -> reads the inverse from cache and returns as output


## makeCacheMatrix function :->
# This function creates a special object that caches the matrix object and its inverse.
# It takes matrix as input and returns a list containing four functions
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of matrix
#get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        
        i  <- NULL
        
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        
        get  <- function(){
                x
        }
        setinverse  <- function(inverse){
                i  <<- inverse
        }
        
        getinverse  <- function(){
                i
        }
        
        list(
                set= set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse
                )
  
}



## cacheSolve function :->
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated before  for the inputted matrix,then the cachesolve 
#retrieves the inverse from the cache, otherwise it computes the inverse and stores in cache for future use.

cacheSolve <- function(x, ...) {
        
        i  <- x$getinverse()
        
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data  <- x$get()
        
        i  <- solve(data, ...)
        
        x$setinverse(i)
        
        i
}
