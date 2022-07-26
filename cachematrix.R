## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){ #function that initialises values of m and inv 
        m <<- y   # using the <<- operator to set the value of m outside the function
        inv <<- NULL
    }
    get <- function() return(m) #function that returns value of m
    setInverse <- function (inverse) inv <<- inverse #function that sets the inverse value to a variable outside this function
    getInverse <- function() inv  #function that returns the inverse value
    slots = list(set = set,
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}
## This function returns the cached value of inverse if available or else finds the inverse and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        print("Getting cached data")
        return(i)
    }
    temp <- x$get()
    i <- solve(temp)
    print(i)
    x$setInverse(i)
    i
}

Tmatrix <- makeCacheMatrix()
Tmatrix$set(matrix(1:4,2,2))
Tmatrix$get() #gets the value of the matrix
Tmatrix$getInverse() #gets the value of the inverse
cacheSolve(Tmatrix)   ## Testing to see if it finds the inverse and returns it 
Tmatrix$get()
cacheSolve(Tmatrix) # Testing it again to see if it uses the cached data

