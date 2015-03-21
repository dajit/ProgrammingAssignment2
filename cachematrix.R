#
## dajit v03.0 21-mar-2015 Addedd version history header
#

## Put comments here that give an overall description of what your
## functions do

## This file contains 2 functions

# 1. makeCacheMatrix
# This functions accpets a matrix as input data. Note that
# it is assumed that the matrix supplied is always invertible as per the
# assignment instructions. This function returns as list
# 4 functions to set, get the input data matrix and setInverser, get Inverse to
# set and get the inverse of the input data matrix


# 2.cacheSolve: This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache

## Write a short comment describing this function
# makeCacheMatrix
# This function return list of functions to
# set input data matrix, get the input data matrix
# store inverse of the input matrix and get/return stored inverse

makeCacheMatrix <- function(met = matrix()) {
  # This function return list if functions to
  # set input data matrix, get the input dat matrix
  # store inverse of aand get/return stored inverse
  
  # met = input data matrix
  # metInv = inverse of the input data matrix
  
  # initialize metInv to a matrix
  
  metInv <- matrix()
  set <- function(m) {
    # store the input data matrix m in  met
    # note that this is set in makeCacheMatrix function
    # using <<- operator
    met <<- m
    # set the inverse metrics to null
    # note that this is set in makeCacheMatrix function
    # using <<- operator
    metInv <<- NULL
  }
  
  # return the metrix stored in makeCacheMatrix
  get <- function() met
  
  # set the inverse matrix in makeCacheMatrix
  # note <<- operator
  setInverse <- function(inverse) metInv <<- inverse
  
  # return the inverse stored
  getInverse <- function() metInv
  
  # return list containing above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
# cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cacheSolve should retrieve the inverse from the cache.
  
  mi <- x$getInverse()
  if(!is.null(mi)) {
    message("getting cached inverse matrix")
    return(mi)
  }
  
  # we reached here means inverse was not cached so calculate inverse
  message("cached inverse matrix not available so calculate inverse")
  dataMatrix <- x$get()
  mi <- solve(dataMatrix, ...)
  x$setInverse(mi)
  
  ## Return a matrix that is the inverse of 'x'
  return(mi)
  
}


# How to use the functions above
# See below for 4 solved examples

# create function to make cache matrix
#> mcm <-  makeCacheMatrix(m)

# Test 1
# create test matrix
#> m1 <- matrix(5:8,2,2)

# set the data matrix
#> mcm$set(m1)
#> mcm$get()
#[,1] [,2]
#[1,]    5    7
#[2,]    6    8
#> 

# now solve the inverse by using cacheSolve function
# note that for the first time the cached inverse is not available
# so the inverse would be calculated and also stored/cached

#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#> 

# for the second run  the inverse stored in cache would be returned
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#> 


#if we change the definition of the matrix then the inverse shoudl be calculated again
#> m1
#[,1] [,2]
#[1,]    5    7
#[2,]    6    8
#> m1 <- matrix(1:4,2,2)
#> m1
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> 

#call the cache matrix again
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#> 

# mcm is still old data
#> mcm$get()
#[,1] [,2]
#[1,]    5    7
#[2,]    6    8
#> 

# now set the input to new matrix in mcm
#> mcm$set(m1)
#> mcm$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> 

# call the solve cache again
#it should calculate new inverse for the new matrix
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 

#now call cacheSolve again, this time cached value shoudld be returned
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 

#Test 2
#> mcm$set(matrix(4:1,2,2))
#> mcm$get()
#[,1] [,2]
#[1,]    4    2
#[2,]    3    1
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> 

#Test 3
#> mcm$set(matrix(4:1,2,2))
#> mcm$get()
#[,1] [,2]
#[1,]    4    2
#[2,]    3    1
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> 

#Test 4

#> set.seed(10)
#> mcm$set(matrix(rnorm(25,mean =50, sd = 10),5,5))
#> mcm$get()
#[,1]     [,2]     [,3]     [,4]     [,5]
#[1,] 50.18746 53.89794 61.01780 50.89347 44.03689
#[2,] 48.15747 37.91924 57.55782 40.45056 28.14713
#[3,] 36.28669 46.36324 47.61766 48.04850 43.25134
#[4,] 44.00832 33.73327 59.87445 59.25521 28.80939
#[5,] 52.94545 47.43522 57.41390 54.82979 37.34802
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1]        [,2]        [,3]        [,4]        [,5]
#[1,] -0.28911542  0.15799440  0.14118338 -0.09777930  0.13374865
#[2,]  0.53557245 -0.32205732 -0.35636555  0.09325079 -0.04801239
#[3,]  0.08948848  0.02130886 -0.03098518  0.04545373 -0.12075397
#[4,]  0.06197419 -0.10334044 -0.05792298  0.05115719  0.03242542
#[5,] -0.49891702  0.30401833  0.38513805 -0.12479954  0.03617791
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1]        [,2]        [,3]        [,4]        [,5]
#[1,] -0.28911542  0.15799440  0.14118338 -0.09777930  0.13374865
#[2,]  0.53557245 -0.32205732 -0.35636555  0.09325079 -0.04801239
#[3,]  0.08948848  0.02130886 -0.03098518  0.04545373 -0.12075397
#[4,]  0.06197419 -0.10334044 -0.05792298  0.05115719  0.03242542
#[5,] -0.49891702  0.30401833  0.38513805 -0.12479954  0.03617791
#> 

#> mcm$set(matrix(rnorm(16,mean =40, sd = 20),4,4))
#> mcm$get()
#[,1]      [,2]     [,3]      [,4]
#[1,] 32.52677 34.924389 43.69852  4.818265
#[2,] 26.24889  2.925191 12.40113 33.509120
#[3,] 22.55682 38.441079 11.28971 26.968740
#[4,] 37.96478 59.371327 47.24174 61.731028
#> cacheSolve(mcm)
#cached inverse matrix not available so calculate inverse
#[,1]         [,2]        [,3]          [,4]
#[1,]  0.019961657  0.035581022  0.03502989 -0.0361759944
#[2,] -0.001196145 -0.023402440  0.02803030  0.0005510474
#[3,]  0.011149252 -0.008573434 -0.04710316  0.0243618358
#[4,] -0.019658401  0.007186536 -0.01245504  0.0192739642
#> cacheSolve(mcm)
#getting cached inverse matrix
#[,1]         [,2]        [,3]          [,4]
#[1,]  0.019961657  0.035581022  0.03502989 -0.0361759944
#[2,] -0.001196145 -0.023402440  0.02803030  0.0005510474
#[3,]  0.011149252 -0.008573434 -0.04710316  0.0243618358
#[4,] -0.019658401  0.007186536 -0.01245504  0.0192739642
#> 
