## Put comments here that give an overall description of what your
## functions do
## ********************************
##  the makeCacheMatrix function returns a list of four functions 
##  to handle an encpsulated matrix. 

        
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

