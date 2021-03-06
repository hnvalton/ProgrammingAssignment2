
## This is a pair of functions that work together. They calculate inverse of
## a matrix and cache the matrix and it's inverse incase the same matrix 
## needs to be inverted again.

## The first function is a list of cache functions needed for 
# The  required caching operations. The functions are: 
# 1. getmatrix1: return the queried matrix (the one we are originally inversing)
# 2. getmatrix2: return cached value of matrix
# 3. setinverse: set the inverse matrix into cache  
# 4. getinverse: get the inverse matrix from cache
# 5. setmatrix2: set the queried matrix into cache

makeCacheMatrix <- function(x = matrix()) {



    #return queried matrix
    getmatrix1 <- function() x           

    #set the matrix in cache
    setmatrix2 <- function(m) mtrx <<- m
   
    #return previous cached matrix if one exists
    getmatrix2 <- function() {
        if(exists("mtrx")){
            mtrx
        }         
    }
    #set the cache of inverse matrix
    setinverse <- function(inverse) invmtr <<- inverse  

    #return the inverse from cache
    getinverse <- function() invmtr
    
    list(getmatrix1 = getmatrix1, getmatrix2 = getmatrix2,
         setinverse = setinverse, getinverse = getinverse,
         setmatrix2 = setmatrix2 )

}


## This function retrieves matrix inverse from cache if it exists for the matrix
# we want to invert. If not found from cache it calculates and returns the 
# inverse. This function also sends matrix and its inverse to cache for future
# queries. 

cacheSolve <- function(x, ...) {

    #queried matrix
    mtr <- x$getmatrix1()       
    #previously cached matrix
    mtr2 <- x$getmatrix2()      

    #if queried and cached matrix are identical get inverse from cache
    # and return it
    if(identical(mtr, mtr2)){
        message("getting inverse matrix from cache")
        inverse <- x$getinverse()
        return(inverse)
    }

    #if not found from cache then solve the inverse and cache matrix and inverse
    inverse <- solve(mtr)
    x$setinverse(inverse)
    x$setmatrix2(mtr)
    inverse
}

