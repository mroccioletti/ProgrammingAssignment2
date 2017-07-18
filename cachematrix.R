## Peer Graded Assignment
## Course: R Programming, Week: 3
## Topic: Lexical Scoping


## makeCacheMatrix creates an object (list) to cache a matrix (data) and its inverse (inv)

makeCacheMatrix <- function(m = matrix()) {

  data <- NULL
  inv <- NULL
  
  ## function to (re)set the cached matrix, its inverse is set to NULL (not computed yet)
  set <- function(d) {
    data <<- d
    inv <<- NULL
  }
  ## function to get the cached matrix
  get <- function() data
  
  ## function to cache the inverse
  setinv <- function(i) inv <<- i
  
  ## function to get the cahced inverse
  getinv <- function() inv
  
  ## set cahced matrix using the incoming parameter m
  set(m)
  
  ## return the object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function takes a parameter of type list representing
## a cached matrix and its inverse created with the function 
## makeCacheMatrix, calculates the inverse and caches the result 
## within the list

cacheSolve <- function(cm) {
  
  ## get inverse from cache object
  inv <- cm$getinv()
  
  ## if NULL, compute it
  if (is.null(inv)) {
    
    ## get data from cache object
    message("Get data...")
    data <- cm$get()
    
    ## compute inverse
    message("Compute result...")
    inv <-solve(data)

    ## cache inverse in cache object
    message("Cache result...")
    cm$setinv(inv)
    
  }
  
  else {
    ## do nothing if inv already cached (not NULL)
    message("Result from cache!")
  }
  
  ## return the inverse
  inv
}
