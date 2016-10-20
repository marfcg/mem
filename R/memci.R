#' generic confidence interval calculation function
#' Part of MEM package
#'
#' \code{i.level.curve} is used for calculating the typical influenza curve,
#' Must be a number between \code{1} and \code{6}:\cr
#' \tabular{rlll}{
#' \tab \code{1} \tab Arithmetic mean and mean confidence interval.\cr
#' \tab \code{2} \tab Geometric mean and mean confidence interval.\cr
#' \tab \code{3} \tab Median and the KC Method to calculate its confidence interval.\cr
#' \tab \code{4} \tab Median and bootstrap confidence interval.\cr
#' \tab \code{5} \tab Arithmetic mean and point confidence interval (standard deviations).\cr
#' \tab \code{6} \tab Geometric mean and point confidence interval (standard deviations).\cr
#' }
#' Option \code{4} uses two more parameters: \code{i.type.boot} indicates which bootstrap
#' method to use. The values are the same of those of the \code{\link{boot.ci}} function.
#' Parameter \code{i.iter.boot} indicates the number of bootstrap samples to use. See
#' \code{\link{boot}} for more information about this topic.\cr
#' Parameters \code{i.level}, \code{i.level.threshold} and \code{i.level.curve} indicates,
#' respectively, the level of the confidence intervals described above.\cr
#'
#' @name memci
#'
#' @param i.data Data frame of input data.
#' @param i.level.curve Level of confidence interval to calculate the modelled curve.
#' @param i.type.curve Type of confidence interval to calculate the modelled curve.
#' @param i.type.boot Type of bootstrap technique.
#' @param i.iter.boot Number of bootstrap iterations.
#' @param i.tails Tails for the confidence interval.
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning
#' and predict the intensity and duration? International Congress Series 1263 (2004)
#' 281-283.\cr
#' Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic
#' thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses,
#' DOI:10.1111/j.1750-2659.2012.00422.x.
#' @export

memci<-function(i.data,i.level.curve=0.95,i.type.curve=1,i.type.boot="normal",i.iter.boot=10000,i.tails=2){
  datos <- i.data
  datos[datos==-Inf]<-NA
  datos[datos==Inf]<-NA
  if(i.type.curve==1){
    return(iconfianza.aritmetica(datos,i.level.curve,ic=T,i.tails))
  }else if(i.type.curve==2){
    return(iconfianza.geometrica(datos,i.level.curve,ic=T,i.tails))
  }else if(i.type.curve==3){
    return(iconfianza.percentil.kc(datos,0.5,i.level.curve,ic=T,i.tails))
  }else if(i.type.curve==4){
    return(iconfianza.percentil.boot(datos,0.5,i.level.curve,ic=T,i.type.boot,i.iter.boot,i.tails))
  }else if(i.type.curve==5){
    return(iconfianza.x(datos,i.level.curve,ic=T,i.tails))
  }else if(i.type.curve==6){
    return(iconfianza.logx(datos,i.level.curve,ic=T,i.tails))
  }
}
