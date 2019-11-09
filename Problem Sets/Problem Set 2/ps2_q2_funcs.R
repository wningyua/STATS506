## Functions for Question 2 of Problem Set 2
##
## Author: Ningyuan Wang
## Updated: Oct 12, 2019

## Functions to normalize the trajectory and compute curves from problem
## set 1.
## Notes: some functions refrer to problem set 1 solution in order to correct
## my own functions and improve running effficency.

trans_to_origin = function(xyt){
  # This function translates an n x 3 matrix of triples, xyt, so the first
  # point is at the origin. We assume the matrix is already ordered by t.
  # 
  # inputs: xyt - a numeric matrix with three columns
  # outputs: the translated matrix
  
  cbind(  xyt[, 1] - xyt[1, 1], xyt[,2] - xyt[1, 2], xyt[ ,3] - xyt[1, 3])  
}

compute_angle = function(xy){
  # compute the angle in radians a point (x, y) forms with the origin and the
  #   x axis. 
  # inputs: xyt a length 2 numeric vector c(x, y)
  # outputs: an angle, in radians, between [-pi/2, pi/2] 
  if ( xy[1] == 0 ) {
    # split special case when x = 0
    return( sign(xy[2]) * pi / 2 )
  } else if ( xy[1] > 0 ) {
    # x > 0
    return( sign(xy[2]) * atan( abs( xy[2] / xy[1] ) ) )
  } else {
    # x < 0
    return( sign(xy[2]) * {pi - atan( abs( xy[2] / xy[1] ) )} )
  }
}

rotate = function(xyt, theta = 0, clockwise = FALSE) {
  # function to rotate the first 2-columns of a coordinate matrix xyt 
  # by theta radians.
  # inputs: xyt - a three column numeric matrix
  #         theta - the angle to rotate xyt[,1:2] 
  #         clockwise - rotate clockwise (defaults to false)
  
  if ( clockwise ) {
    s = 1
  } else {
    s = -1
  }
  R = matrix( c(cos(theta), -s*sin(theta), s*sin(theta), cos(theta) ), 2, 2 )  
  
  ## Apply the rotation
  cbind( xyt[ , 1:2] %*% R, xyt[ , 3] )
}

normalize_traj = function(xyt) {
  # This function normalizes the trajectory in xyt to start at the origin and
  # conclude along the positive x-axis.
  #
  # Inputs: 
  #   xyt - an n x 3 numeric matrix with time in the final column
  # Output:
  #   A matrix with the same dimensions as xyt representing the normalized
  #   trajectory.  
  
  # check input
  stopifnot( is.numeric(xyt) ) 
  stopifnot( ncol(xyt) == 3 )
  
  # Compute the rotation agnle
  theta = compute_angle( xyt[nrow(xyt), 1:2] - xyt[1, 1:2] )
  
  # Translate and rotate to normalize
  rotate( trans_to_origin(xyt), theta = theta )
}

comp_dist = function(x, y) {
  # compute the total distance between successive (x, y) pairs
  # inputs: x, y - numeric vectors of the same length
  # output: a numeric constant with the distance traveled along the trajectory 
  #         (x, y)
  stopifnot( length(x) == length(y) )
  sum( sqrt( diff( x )^2 + diff( y )^2 ) ) 
}

comp_auc = function(x, y, absy = TRUE){
  # compute the area under the curve traced out by x, y
  # allowing cancellation in x and (optionally) y. 
  # inputs: x, y - numeric vectors of the same length
  #         abs - should we use the absolute value of y or allow cancellation?
  # output: .5 * sum_i (|y_i| + |y_i+1|)*(x_i+1 - x_i)
  stopifnot( length(x) == length(y) )
  
  dx = diff(x)
  if ( absy ) {
    return( .5 * sum( {abs( y[-length(y)] ) + abs( y[-1] )} * dx ) )
  } else {
    return( .5 * sum( {y[-length(y)] +  y[-1]} * dx ))
  }
  
}

comp_measures = function(xyt) {
  # This function computes the following curvature measures for a normalized
  # trajectory xyt given as an n x 3 numeric matrix. Each row  of xyt should 
  # give the coordinates in the the x, y at time t (in that order).
  
  # Inputs: 
  #  xyt - a numeric matrix with three columns
  # Output: 
  #  A named vector with the following curvature measures:
  #    dist - the total (Euclidean / L2) distance traveled along the trajectory
  #    max_abs_dev - the maximum absolute deviation from the secant line 
  #                  representing a direct path from the first to last point.
  #    avg_abs_dev - the average abosolute deviation form the secant line. 
  #    auc - the absolute area under the curve, computed with the trapezoidal rule
  #          and allowing cancelation in the "x" dimension. 
  
  # Test the input  
  stopifnot( is.numeric(xyt) )
  stopifnot( ncol(xyt)  == 3 )
  if ( !all( xyt[1, ] == 0 ) || 
  {abs( xyt[nrow(xyt), 2] ) > sqrt( .Machine$double.eps )} ) {
    stop("Trajectory is not normalized. Use normalize_traj() first.\n")  
  }
  
  # Compute the curvature measures and return them
  c(
    'tot_dist' = comp_dist(xyt[, 1], xyt[, 2]),
    'max_abs_dev' = max( abs(xyt[, 2]) ),
    'avg_abs_dev' = mean( abs(xyt[, 2]) ),
    'AUC' = comp_auc(xyt[, 1], xyt[, 2])
  )
  
}
