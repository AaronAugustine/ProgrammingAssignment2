## This code has 2 functions  
## makeCacheMatrix which creates a special matrix object that can cache its inverse.
## cacheSolve which calculates the inverse of the special matrix.
## Detailed comments are provided in the functions
## At the bottom of the code is example commands to test the functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                           #Assign null to i    
  set <- function(y) {
    x <<- y                           #Assign y to x: note the << operator
    i <<- NULL                        #Assign null to i: note the << operator
  }
  get <- function() x                 #Function Get x
  setinv <- function(inv) i <<- inv   #Function set i to inv: note: inv is the function variable
  getinv <- function() i              #Function Get i
  list(set = set, get = get,          #Put all these function in a list
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  i <- x$getinv()                      #Use getinv function to get inverse from the list
  if(!is.null(i)) {                    #If inverse is not null return it
    message("getting cached data")
    return(i)                          #Return the value of i and exit the function
  }
   data <- x$get()                     #use get function pull matix off the list            
   i <- solve(data, ...)               #Calculate the inverse of data as assign to i
   x$setinv(i)                         #Use setinv function to assign the inverse to the list
   i                                   #return the inverse from the function
}

#Code to test the functions
#mdat <- matrix(c(100,200,300,400), nrow = 2, ncol = 2, byrow = TRUE)  Create a test matrix
#x<-makeCacheMatrix(mdat)                                              Assign the special matrix to x
#cacheSolve(x)                                                         Find the inverse
#y<-cacheSolve(x)                                                      Make sure the "getting cached data message" works
#mdat%*%y                                                              Verify you get the identity matrix back