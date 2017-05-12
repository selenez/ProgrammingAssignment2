## These functions take a matrix and calculate its inverse and store it
## so the result can be recalled without having to calculate it again


## This function takes a matrix and converts it into a cacheable one.
## To acomplish this, it carries out four tasks
## 1.- the set function stores the input data
## 2- the get function returns the original data
## 3.- the setinv function stores the inverse of the matrix
## 4.- the getinv fuction returns the stored inverse
## It returns a list of the four objects desribed above


makeCacheMatrix <- function(x = matrix()) {
  	inv <-NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function(){
		x
	}	
	setinv<- function(solve){
		inv<<- solve
	}
	getinv<- function(){
		inv
	}
	list(set = set, get =get, setinv=setinv, getinv =getinv)
}



## This function takes a cacheable matrix and looks for a stored inverse matrix in $getinv
## If a stored inverse matrix exists it prints a message informing that and returns the inverse.
## If not, it gets the data from $get, uses solve to calculate the inverse,
## stores it using $setinv and returns the inverted matrix

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
	if (!is.null(inv)){
		message('getting cache data')
		return(inv)
	}
	data<- x$get()
	inv<-solve(data, ...)
	x$setinv(inv)
	inv
}
