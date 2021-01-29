##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ivrs<- NULL
        setMX<- function(w) {
                x<<-w
                ivrs<<-NULL
        }
        getMX<- function(){x}
        setinv<- function(inv) {ivrs<<- inv}
        getinv<- function() {ivrs}
        list(setMX=setMX, 
             getMX=getMX, 
             setinv=setinv, 
             getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ivrs<- x$getinv()
        if(!is.null(ivrs)){
                message("getting cached data")
                return(ivrs)
        }
        MX<- x$getMX()
        ivrs<- solve(MX, ...)
        x$setinv(ivrs)
        ivrs
}


##TEST
examplematrix<- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
examplematrix$getMX()
examplematrix$getinv()
testsolve<- cacheSolve(x=examplematrix)
testsolve
testsolve<- cacheSolve(x=examplematrix)
examplematrix$getinv()
