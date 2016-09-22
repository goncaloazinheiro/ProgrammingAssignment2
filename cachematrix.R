## makeCacheMatrix is a function that allows:
#1.set the matrix
#2.get the matrix
#3.set the inverse of the matrix
#4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        
        setmatrix<-function(y){
                x<<-y
                m<<-NULL
                
        } 
        
        getmatrix<-function() x
        
        setmatrixinv<-function(solve) m<<-solve
        
        getmatrixinv<-function() m
        
        list(setmatrix=setmatrix,getmatrix=getmatrix,setmatrixinv=setmatrixinv,getmatrixinv=getmatrixinv)
        
}


#The following function calculates the inverse of the matrix
#it first checks to see if the inverse matrix has already been calculated 
#for a variable. 
#If so, it gets the inverse matrix from the cache and skips code. 
#Otherwise, it calculates the inverse matrix and sets inversed matrix 
#in the cache via the setmatrixinv function. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inversedmatrix <- x$getmatrixinv()
        if(!is.null(inversedmatrix)) {
                message("Matrix from cached data")
                return(inversedmatrix)
        }
        data <- x$getmatrix()
        inversedmatrix <- solve(data)
        x$setmatrixinv(inversedmatrix)
        inversedmatrix
}
