##Functions that can be used to create a cache of the inverse of a matrix

##Function to create a matrix that can cache the inverse
makeCacheMatrix<-function(m=matrix()){

##Assigns NULL to value i
		i<-NULL

##Function that will create the matrix
		set<-function(matrix)
			m<<-matrix
			i<<-NULL
}

##Function that will get the matrix
		get<-function(){
			m
}

##Function that will apply the inverse of the matrix
		setinverse<-function(){
			i<<-inverse
}

##Function that will get the inverse of the created matrix
		getinverse<-function(){
			i
}

##Will return a list of results created by the functions
		list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##Function to determine the results of the inverse of the special matrix 
##returned by makeCacheMatrix. When the inverse has already been calculated, 
##the cacheSolve will use the cache to determine the result.
cacheSolve <- function(x, ...) {

##Gets the matrix that is the inverse of x
		m<-x$getinverse()

##Return the data if the matrix has been cached
		if(!is.null(m)){
			return(m)
}

##Return the matrix
		data<-x$get()

##Return results of the inverse matrix
		m<-solve(data)%*%data

##Sets the inverse to the matrix
		x$setinverse(m)

##Returns the result of the matrix
		m
}

