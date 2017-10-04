 makeCacheMatrix <- function(x = matrix()) {
+         inv <- NULL
+         set <- function(y) {
+                 x <<- y
+                 inv <<- NULL
+         }
+         get <- function() x
+         setInverse <- function(inverse) inv <<- inverse
+         getInverse <- function() inv
+         list(set = set,
+              get = get,
+              setInverse = setInverse,
+              getInverse = getInverse)
+ }
> 
> cacheSolve <- function(x, ...) {
+         ## Return a matrix that is the inverse of 'x'
+         inv <- x$getInverse()
+         if (!is.null(inv)) {
+                 message("getting cached data")
+                 return(inv)
+         }
+         mat <- x$get()
+         inv <- solve(mat, ...)
+         x$setInverse(inv)
+         inv
+ }
> m <- makeCacheMatrix(matrix(1:5, 2, 2))
> m$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
Warning message:
In matrix(1:5, 2, 2) :
  data length [5] is not a sub-multiple or multiple of the number of rows [2]
> m$getInverse()
NULL
> cacheSolve(m)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
