## makeCacheMatrix gives a matrix x four functions to manipulate, the operator "<<-" has not
## been defined accurately, so the functions in this file may not really work 
## cacheSolve tries to retrieve the inverse of matrix x from some cached zone. And if it fails
## it will calculate the inverse of x and store newly got value in the cached zone.     

makeCacheMatrix <- function(x = matrix()) {
     Inver<-NULL  ## initialization of Inver
	 set<-function(y){
	         x<<-y
			 Inver<<-NULL
	  }
	 get<-function(){x} # the function "get" requires no arguments, returns x
	 setInverse<-function(Inverse_vec){Inver<<-Inverse_vec}
	 getInverse<-function(){Inver} # require no arguments, return Inver
	 
	 list(set=set,get=get,        
	      setInverse=setInverse,
		  getInverse=getInverse)

}


cacheSolve <- function(x, ...) {
        Inver<-x$getInverse()  ## try to get the inverse matrix of x from the cached zone
		if(!is.null(Inver)){   ## if found the cached matrix
		        message("getting cached data")
				return(Inver)    ## return the inverse matrix of x
		}
		data<-x$get()  
		Inver<-solve(data,...) # the real calculate step of the function  
		x$setInverse(Inver)
		Inver  ## Return a matrix that is the inverse of 'x'
}
