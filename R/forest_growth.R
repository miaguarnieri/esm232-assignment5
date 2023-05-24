#'  Forest growth model
#' @param time time since start
#' @param C forest carbon
#' @param parms - as list with three values, r, K, closure
#' @param r intrinsic growth rate (linear growth rate once canopy closure has been reached)
#' @param K carrying capacity (kgC)
#' @param g linear growth rate after closure
#' @param closure canopy closure (kg C)
#' @return derivative of population with time 

forest_growth <- function(time, C, parms) {
  
  dC = ifelse(C < parms$closure, parms$r*C, parms$g*(1 - C/K))
  
  dC = ifelse(C >= parms$K, 0, dC)
  
  return(list(dC))
}