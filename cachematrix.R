## =======================================================================================
#  makeCacheMatrix is called with one argument, an invertible matrix, e.g.,
#    a <- makeCacheMatrix(matrix(c(1,0,3,2,2,4,3,2,1),ncol=3))
#
#  makeCacheMatrix returns a list were each elments is a pointer to function.
#    
#    a is a list that points to 4 functions. To use these function from the 
#    command line in the top level workspace, call with the 'external' name 
#    defined in the call to list (see comments inside makeCacheMatrix)
#      a$setmagetrix(matrix)              - assigns a new matrix to top level local var 'm'
#      a$getmatrix()                    - returns value of top level local var 'm'
#      a$setinvmatrix(invmatrix)        - resets the inverse value of m
#      a$getinvmatrix()                 - returns the value of the inverse matrix
## =======================================================================================
#  Special Notes:
#    the value of the matrix in the call to makeCacheMatrix "lives" in the 
#    local space of getmatrix(), as the local (to the function makeCacheMatrix) variable 'm.'
## =======================================================================================
makeCacheMatrix <- function(m = matrix()){
        im <- NULL # holds the inverse
        
        # Start define function setmatrix -----------------------
        #       resets the value of the main function arg 'm'
        setmatrix <- function(y) {
                m <<- y # local y assigns a new matrix to the top level local variable 'm'
                im <<- NULL
        }
        # End define function set -------------------------
        
        # Start define function get -----------------------
        #       returns the matrix, the value of the main function arg 'm'
        getmatrix <- function() m
        # End define function get -------------------------
        
        # Start define function setmatrixinv -----------------------
        #       resets the value of the main function arg 'im'
        setinvmatrix <- function(invmatrix) im <<- invmatrix
        # End define function setmatrixinv   -----------------------
        
        # Start define function getinvmatrix -----------------------
        #    returns the inverse matrix, he value of the main function arg 'im'
        getinvmatrix <- function() im
        # End define function getinvmatrix   -----------------------
        
        # return a list of pointers to these functions
        # setup is: external function name = internal function
        list(set = setmatrix, get = getmatrix,
             setinv = setinvmatrix,
             getinv = getinvmatrix)
}

## =======================================================================================
# cacheSolve's arg is a list of pointers to the functions created by the call to makeCacheMatrix.
# cacheSolve first tries to retrieves the value of the inverse matrix (im).
# if if can, it just returns it's value with the msg "getting cached data"
# if it can't, then it retrieves the value of the matrix, inverts it, and then stores (caches) its value
## =======================================================================================
cacheSolve <- function(a, ...) {
        im <- a$getinv() # get the inverse matrix
        if(!is.null(im)) {
                # if im exist, just return its value
                # if im is null, execute the code after the if {}
                message("getting cached data")
                return(im)
        }
        # executed if im is null (no inverse matrix)
        data <- a$get()         # get the matrix
        im <- solve(data, ...)  # compute its inverse
        a$setinv(im) # where the inverse matrix of makeCacheMatrix get cached 
        im
}