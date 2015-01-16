## Put comments here that give an overall description of what your
## functions do
# This functions are created as a Programing assignment 2 durig the
# course in R programing on Coursera with course code rprog-010
# They are create in order to exploit powers of lexical scoping
# in order to cache the results which are time consuming to compute
#
# Functions make cachable matrix and calculate its inverse

## Write a short comment describing this function
# This function is used to make cachte matrix
# It initializes the inverse and creates
# set and get methods for the matrix itself and its inverse
#
makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse to null in order to indicate that its
    #   value has not bee assigned yet
    inverse <-NULL
    
    #function seting the value of the matrix
    set<- function( y )
    {
        #save the value
        x<<- y;
        #indicate that inverse is not calculate
        inverse<<-NULL
    }
    get<- function() x
    
    # function to set the inverse once it has been calculated
    setinverse<- function(y)
    {
        inverse<<-y
    }
    
    # function that gets the inverse
    getinverse<- function() inverse
    
    list(set=set,get= get,setinverse= setinverse,getinverse= getinverse)

}


## Write a short comment describing this function
# This function is used to calculate and cache inverse matrix
# if the inverse has not been calculated yet calculates its value
# if the inverse has been calculated that value is returne
cacheSolve <- function(x, ...) {
        ## get the inverse
        iv=x$getinverse()
        
        # if it is not calculated
        if (is.null(iv))
        {
            #calculate inverse
            iv=solve(x$get())
            message("Caching inverse matrix")
            #set the inverse
            x$setinverse(iv)
        }
        #return the result
        iv
}
