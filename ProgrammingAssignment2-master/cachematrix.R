## the function is checking if an inverse exists, if it doesn't, it assigns a null value to it

##to run the function 
## step 1 -> assign a square matrix value to x e.g. x<-matrix(rnorm(1:9,3,2),3,3)
##step 2 -> output <-makeCachematrix(x)
##step 3 -> output$set(x)
##step 4 -> catcheSolve(output)
## to run again you can repeat step 4 or change value of run and run step 3 and step 4

## the function is first helping assign values to the matrix and its inverse and storing them together. 
## when you run the function, it checks if inverse is already available... if yes, it prints the inverse value
## if x has changed, it recomputes the inverse and prints the same

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## the function checks if an inverse exists, if it does, it returns a cached value, else it computes inverse & returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#to run the function 
# step 1 -> assign a square matrix value to x e.g. x<-matrix(rnorm(1:9,3,2),3,3)
#step 2 -> output <-makeCachematrix(x)
#step 3 -> output$set(x)
#step 4 -> catcheSolve(output)
# to run again you can repeat step 4 or change value of run and run step 3 and step 4
