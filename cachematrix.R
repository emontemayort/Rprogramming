## I created a function where you input a matrix and first it sets inverse value to NULL and sets the value of x to y
## Then get returns the matrix x and setinverse sets the value of the inverse of the matrix to inv
## getinverse returns the inv matrix, the we list all these functions we just defined

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function returns the inverse of the matrix
## if the inverse given by the function above is NULL 
## then it calculates the inverse of the matrix and returns it

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinverse(inv)
        inv
}
