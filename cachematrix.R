## makeCacheMatrix and cacheSolve are a series of functions that creates a
## special object to save the value of a matrix and its inverse. 
## This allows us to look up the value of the inverse from the cache, instead of
## having to compute the inverse repeatedly

## FUNCTION 1: makeCacheMatrix

## makeCacheMatrix(x) creates the following list of functions from the Matrix x:
## 1. Set the cached value of the Matrix
## 2. Get the cached value of the Matrix
## 3. Set the cached value of the Inverse of the Matrix
## 4. Get the cached value of the Inverse of the Matrix
## (Assuming x is a invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
       
        ## Intialise the value of the Inverse to NULL, as the Matrix has yet to 
        ## be solved
        inv <- NULL
        
        ## Sets a new value of the Matrix and initialises the Inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Gets the value of the Matrix
        get <- function() x
        
        ## Sets the value of the Inverse to a user-defined value
        setinverse <- function(solve) inv <<- solve
        
        ## Gets the cached value of the Inverse
        getinverse <- function() inv
        
        ## returns the list of associated functions to set/get the cached values
        ## of the Matrix and its Inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## FUNCTION 2: cacheSolve

## cacheSolve(y) does one of the following:
## 1. Returns the Inverse of the Matrix from the cache;
## OR, if the Inverse has not been calculated and cached,
## 2. Calculates the Inverse of the matrix and saves it in the cache
## (Assuming y is a list of functions obtained from makeCacheMatrix(x))

cacheSolve <- function(y, ...) {
        ## obtains the Inverse of the Matrix from the cache
        inv <- y$getinverse()
        
        ## Check if there is a value for the Inverse in the cache, and returns
        ## the cached value of the Inverse if it exists
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If the Inverse has not been cached, solve for the Inverse of the 
        ## Matrix and save it in the cache
        data <- y$get()
        inv <- solve(data)
        y$setinverse(inv)
        
        ## Return the calculated Inverse of the Matrix
        inv
}
