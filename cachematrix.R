## Put comments here that give an overall description of what your
## functions do
## ********************************
##  the makeCacheMatrix function returns a list of four functions 
##  to handle an encpsulated matrix. 
##  One of them allows to get the inverse of the current matrix
##  However it returns NULL if the makeCacheMatrix function has not been called
##  before.

##  Indeed,the makeCacheMatrix function is in charge of calculating only once,
##  the inverse of the given makeCacheMatrix object as its argument.
##  When this function is called twice and more, it returns a caching result, 
##  thus to increase the performance. It's particularly fine
##  when this function handles big square matrix.
        
## ********************************
## Write a short comment describing this function
##      This function creates a special matrix which is a list containing
##      the following functions :

##    setMat() : set the matrix to encapsulate, a matrix is given as an argument  
##    getMat() : returns the value of the encapsulated matrix 
##    setInv() : set the inverse of the matrix
##    getInv() : get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        setMat<-function(m){
                x<<-m
                inv<<-NULL
        }
        getMat<-function() x
        setInv<-function(solve) inv <<-solve
        getInv<-function() inv        
        list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)
        
}


## Write a short comment describing this function
##  makeCacheMatrix calculates the inverse of the makeCacheMatrix object given
##  as its argument.
##  When cacheSolve is called for calculating the inverse of this given matrix,
##  it first calls the getInv() function upon this matrix object so as to verify
##  if this instance has its inverse already calculated.
##  If this is the case, cacheSolve only returns this caching result, 
##  otherwise it calculates for the first time the inverse
##  of this makeCacheMatrix object.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$getMat()
        inv<-solve(data)
        x$setInv(inv)
        inv
        
}

