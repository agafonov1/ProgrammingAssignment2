## file has definition of cacheMatrix function and cacher function
## to allow inverse matrix caching

## Allows to create cacheMatrix - methods to set and get matrix 
## and get and set cache
makeCacheMatrix <- function(Matrix = matrix()) {
  Cached_Inverse_matrix<-NULL
  set<-function(Input_matrix){
    Matrix<<-Input_matrix
    Cached_Inverse_matrix<<-NULL
  }
  get<-function() Matrix
  getInverse<-function() Cached_Inverse_matrix
  setInverse<-function(Inversed_inp) Cached_Inverse_matrix<<-Inversed_inp
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}
##Creates a chached Inverse cacheMatrix. 
##If cache exists, returns it, 
##otherwise calculates inverse matrix and srotes it in cacheMatrix
cacheSolve <- function(x, ...) {
  mi<-x$getInverse()
  if(!is.null(mi)){
    message("GETTING CACHE DATA")
    return(mi)
  }
  message("creating cache")
  mx<-x$get()
  mi<-solve(mx,...)
  x$setInverse(mi)
  mi
  ## Return a matrix that is the inverse of 'x'
}
