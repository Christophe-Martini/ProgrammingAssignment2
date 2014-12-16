## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        revMat<-NULL
        setMat<-function(m){
                x<<-m
                revMat<-NULL
        }
        getMat<-function() x
        setRev<-function(solve) revMat<<-solve
        getRev<-function() revMat        
        list(setMat=setMat,getMat=getMat,setRev=setRev,getRev=getRev)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        revMat<-x$getRev()
        if(!is.null(revMat)){
                message("getting cached data")
                return(revMat)
        }
        data <- x$getMat()
        revMat<-solve(data)
        x$setRev(revMat)
        revMat
        
}

