#This function creates object that cache its inverse
makeCacheMatrix<-function(x=matrix()){
	inv<-NULL
	set<-function(funtion(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setInverse<-funtion(inverse) inv<<- inverse
	getInverse<-funtion() inv
	list(set=set,
		 get=get,
		 setInverse=setInverse,
		 getInverse=getInverse)
	}
}

#This function computes the inverse of the matrix from the cache,if inverse has already been created. 
cacheSolve<-function(x, ...){
	inv<- x$getInverse()
	if(!is.null(inv)){
		message ("Using Cached Data")
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat, ...)
	x$setInverse(inv)
	inv
}
