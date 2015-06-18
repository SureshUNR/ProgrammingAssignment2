## makeCacheMatrix creates a special matrix vector, and then cacheSolve 
## calculates the inverse of that matrix.
## If the matrix inverse has already been calculated, it will  
## find it in the cache and return the cached result without calculating it again thus saving on 
## computing resources.




# makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
    
    #inv is a variable that stores the inverse, Initially it is set to null
    inv <- NULL 
    
    # set function sets variable x to matrix y. y is supplied as an argument to the set function.
    # <<- operator is used as both x and inv are not defined in the set function but the makeCacheMatrix function.
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    } 
    
    
    # get function return the input matrix stored in the variable x.
    get <- function() {
        x 
    }
    
    
    # variable inv is set to the inversed matrix. The inversed matrix is supplied as an argument to the function setinverse.
    # operator <<- is used as inv is not defined in the setinverse function but the makeCacheMatrix function.
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    
    # return the inversed matrix
    getinverse <- function() {
        inv 
    }
    
    
    # return a list that contains these functions, so that we can use
    # makeCacheMatrix object like these
    # mat <- makeCacheMatrix(InputMatrix)
    # mat$set(newmatrix) # to change matrix
    # mat$get() # to get the setted matrix
    # mat$setInv(invertedMatrix) # to set the inversed matrix
    # mat$getInv() # to get the inversed matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
    
}




# Below function will compute the inverse of a matrix
# and cache the result.
# When the function is called with a special matrix returned by makeCacheMatrix
# it will first search the cache using getinverse() function of the special matrix.
# if getinverse() function does not return the cached result.
# the function goes ahead and calculates the inverse using solve()
# and then sets it in the cache of the special matrix using setinverse() function.


cacheSolve <- function(x, ...) {
    
    # get the inversed matrix from object x
    inv <- x$getinverse() 
    
    
    if(!is.null(inv)) { 
        # if the inverse of the matrix is there
        message("getting cached data.") 
        # return the cached result
        return(inv) 
    } 
    
    # Since the inverse of a matrix is not cached, we will first get the matrix using x$get 
    data <- x$get() 
    
    # we find the inverse of a matrix using the solve function. Note that the matrix needs to be square matrix 
    # so that we can use the solve function.
    inv <- solve(data,...) 
    
    
    # we set the inverse of the matrix computed above to the vector x. 
    x$setinverse(inv) 
    
    # return the inverse of a matrix 
    inv 
    
}
