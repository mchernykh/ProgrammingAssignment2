## Put comments here that give an overall description of what your
## functions do

# sample:
# > A = matrix(c(2,0,0,2), nrow = 2, ncol = 2)
# > pA = makeCacheMatrix(A)
# > ls(pA)
# [1] "get"        "getinverse" "set"        "setinverse"
# > cacheSolve(pA)
     # [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# > cacheSolve(pA)
# getting cached data
     # [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# > ls(environment(pA$set))
# [1] "get"        "getinverse" "input"      "inverse"    "set"        "setinverse"


## creates object for cacheSolve
## copy-pasted from https://class.coursera.org/rprog-016/human_grading/view/courses/973757/assessments/3/submissions
## variables were renamed in hunt of understanding

makeCacheMatrix <- function(input = matrix()) {
	inverse <- NULL
	set <- function(y) {
		input <<- y
		inverse <<- NULL
	}
	get <- function() input
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## gives inverse matrix of matrix set by $set function. takes it from cache if possible
## copy-pasted from https://class.coursera.org/rprog-016/human_grading/view/courses/973757/assessments/3/submissions
## variables were renamed in hunt of understanding

cacheSolve <- function(input, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- input$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- input$get()
	inverse <- solve(data, ...)
	input$setinverse(inverse)
	inverse
}
