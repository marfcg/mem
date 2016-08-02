#' find first TRUE value function, returning NA if not present
#'
#' @keywords internal
primer.verdadero <- function(x){
  ifelse(sum(x)>0, return(which.max(x)), return(NA))
}
