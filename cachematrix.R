## This code has 2 functions 
## makeCacheMatrix which creates a special matrix object that can cache its inverse.
## cacheSolve which calculates the inverse of the special matrix.
## Details comments are provided in the function

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
