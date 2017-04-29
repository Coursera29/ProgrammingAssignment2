## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a list of four functions for the data matrix that is passed.
# Two functions set and get the data matrix itself
# Two functions set and get the cached inverse of the data matrix 

makeCacheMatrix <- function(x = matrix()) 
{
        #setting initial inverse value as Null 
        inverse_val <- NULL
        
        #setting new value if new values are assigned to the matrix
        setDataMatrix <- function(newData) 
        {
                x <<- newData
                inverse_val <<- NULL
        }
        
        #function to get the matrix
        getDataMatrix <- function() x
        
        #function to assign inverse
        setInverseValue <- function(inverse) inverse_val <<- inverse
        
        #function to obtain the invesre value
        getInverseValue <- function() inverse_val
        
        # making and returning a list of the aforementioned functions
        list(
                setDataMatrix = setDataMatrix, 
                getDataMatrix = getDataMatrix,
                setInverseValue = setInverseValue,
                getInverseValue = getInverseValue
        )
}


## Write a short comment describing this function

#This function first obtains the DataMatrix argument and checks if its inverse already exists 
# using the getInverseValue function , if it does, then that retrieved inverse is returned else
# the inverse value is calculated again using the solve function ( I have added the solve 
# function just in case to test).

cacheSolve <- function(x, ...) 
{
        #Get the preCalculated inverse, if applicable
        Inverse <- x$getInverseValue()
        
        #if inverse has already been calculated , it will be not null
        if(!is.null(Inverse)) 
        {                
                print("Cached Inverse exists! Obtaining it...")
                return(cachedInverse)
        }
        
        print("Cache doesn't exist..")
        
        #Since cache doesn't exist, obtain the data matrix
        matrixData <- x$getDataMatrix()
        
        #calculate the fresh inverse
        #solve function returns the inverse of this matrix
        Inverse <- solve(matrixData)
        
        #Set the calculated inverse to the list's cached value 
        # so that can be retrieved later on.
        
        x$setInverseValue(Inverse)
        
        ## Return a matrix that is the inverse of 'x'
        Inverse
        
}

# Function that calculates the inverse in this case of passed data matrix
solve <-function(x,...) {
        1/x
        
}
