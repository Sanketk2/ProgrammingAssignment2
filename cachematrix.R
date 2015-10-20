makeCacheMatrix = functions(x=matrix())        # Essentially creating a list containing function to set and get the vaues of the matrix and its inverse.
{
  m = NULL
  set = function(y)
  {
    x<<-y
    m<<-NULL
  }
  get = function() x
  set_inverse = function(inv) m<<-inv
  get_inverse = function() m
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

cacheSolve = function(x,...)                  # Computes the inverse of the special matrix created above, if it gets the mean from the cache, it'll skip the computation.
{
  m = x$get_inverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  data = x$get()
  m = solve(data, ...)
  x$set_inverse(m)
  m
}