
 makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeVector <- function(x = numeric()) {
+        m <- NULL
+        set <- function(y) {
+                x <<- y
+                m <<- NULL
+        }
+        get <- function() x
+        setmean <- function(mean) m <<- mean
+        getmean <- function() m
+        list(set = set, get = get,
+             setmean = setmean,
+             getmean = getmean)
 }
 
 
#The following function returns the inverse of the matrix. It checks first if
#the inverse has already been calculated. If it holds true, it gets the result and omits the
#calculation. If it holds false, it calculates the inverse, sets the value in the cache via
#setinverse function.
#This function assumes that the matrix is always invertible.

 cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
+    inv <- x$getinverse()
+    if(!is.null(inv)) {
+        message("getting cached data.")
+        return(inv)
+    }
+    data <- x$get()
+    inv <- solve(data)
+    x$setinverse(inv)
+    inv
 }
