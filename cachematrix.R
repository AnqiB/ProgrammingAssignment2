## makeCacheMatrix gives a matrix x four functions to manipulate.
## cacheSolve tries to retrieve the inverse of matrix x from the environment in which the two functions are defined. And if it fails
## it will calculate the inverse of x and store newly got value in the cached zone.
## '<<-' has been already defined in R.

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

## The following test example and method is provided by Angelo Peñaloza Salazar on the class forum. 
#This is a test example to evaluate functions makeCacheMatrix and cacheSolve

#1. Run both functions on Rstudio console

#2. Define a 2x2 matrix called M
#> M <- matrix(1:4, ncol=2, nrow=2)

#3. Store the value of running makeCacheMatrix(M) which is a list.
#> cacheM <- makeCacheMatrix(M)

#4. Compute, cache, and return the inverse of matrix M
#> cacheSolve(cacheM)

#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#5. Show the original matrix stored
#> cacheM$get() 

#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#6. Show the inverse of the matrix stored
#> cacheM$getInverse()

#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#7. Show the inverse of M called from cache
#> cacheSolve(cacheM)  

#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


