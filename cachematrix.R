## What: Programming Assignment 02 - Programming in R
## Date: 17 Sep 2014
##
## Description:
## This file contains two functions - makeCacheMatrix and cacheSolve.
## The first function creates a "special" container "matrix" that 
## allows one to get/set the matrix content, but also to get/set the
## inverse of the matrix.
## The second function uses an instance of a "special" container "matrix"
## created by the first function and either calculates the inverse matrix 
## (and stores it in the cache) or retrieve it from "cache".


## Function: makeCacheMatrix
##
## Parameters: 
## x - "seeding" matrix
##
## Return: A list, containing:
## get - function to return the current matrix
## set - function to set the current matrix, and to clear the cache entry
## getinv - function to return the current inverse matrix from cache
## setinv - function to set the current inverse matrix in the cache
##
## Description: This function creates a "special" matrix that can cache it's
## inverse. As per assignment stipulations, it is assumed that m will always
## be invertable.
makeCacheMatrix <- function(x = matrix()) {
    ## Invers Matrix - for cache purposes
    mInv <- NULL
    
    ## set function - set new matrix AND clear inverse matrix from cache
    set <- function(mNew) {
        x <<- mNew
        mInv <<- NULL
    }
    
    ## get function - get current matrix
    get <- function() {
        x
    }
    
    ## setin function - set new inverse matrix to cache
    setinv <- function(mInvNew) {
        mInv <<- mInvNew
    }
    
    ## getinv function - get current inverse matrix from cache
    getinv <- function() {
        mInv
    }
    
    ## return list of four functions above to access cache and matrix
    list(set=set,get=get,setinv=setinv,getinv=getinv) 
}


## Function: cacheSolve
##
## Parameters: 
## x - "special" matrix created by makeChacheMatrix
## ... - open "list" of paraters.
##
## Return: The inverse of "special" matrix x
##
## Description: This function computes the inverse of a special "matrix"
## returned by function makeChacheMatrix. If the inverse has allready been
## calculated (and the matrix has not changed), then the inverse matrix is
## retrieved from the cache.
cacheSolve <- function(x, ...) {
    ## get the current inversre matrix from cache
    mInv <- x$getinv()
    
    ## if inversematrix NOT null, return
    if(!is.null(mInv)) {
        message("Cache NOT empty - Getting cached inverse matrix...")
        
        ## return inverted matrix
        return(mInv)
    }
    
    ## In the event that inversematrix IS null (i.e. cache is empty),
    ## calculate new inverse matrix
    message("Cache empty - calculate inverse matrix...")
    
    ## get non-inverted matrix
    data <- x$get()
    
    ## invert matrix
    mInv <- solve(data, ...)
    
    ## set new inverted matrix in cache
    x$setinv(mInv)
    
    ## return inverted matrix
    mInv     
}
