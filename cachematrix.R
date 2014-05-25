##This function creates a special "matrix" object that can cache its matrix inverse.
##The matrix object is dynamic and can be defined in an argument.
##The matrix value is set, the values are inputted into the matrix, 
##then the inverse matrix is set, then it gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
     	set <- function(y) {
                x <<- y
                m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##This function computes the inverse of the special matrix returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##and the matrix has not changed, then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
 	m <- x$getsolve()
      if(!is.null(m)) {
                message("getting cached data")
                return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}


##Test solution with 2x2 default matrix

	##initialize matrix
a <- makeCacheMatrix() 

	##set the matrix
a$set(matrix(1:4,2,2))  
   
	##get the matrix 
a$get()     
         
	##calculate the inverse      
cacheSolve(a)  

	##when is called back use the cached inverse    
cacheSolve(a)            


##Test solution with 2x2 chosen matrix

	##initialize matrix
a <- makeCacheMatrix() 

	##set the matrix
a$set(matrix(c(8, 3, 2, 1), nrow=2, ncol=2)) 
   
	##get the matrix 
a$get()     
         
	##calculate the inverse      
cacheSolve(a)  

	##when is called back use the cached inverse    
cacheSolve(a)
 
