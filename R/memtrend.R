#' @title Methods for influenza trend calculation
#'
#' @description
#' Function \code{memtrend} is used to calculate the two parameters for defining the
#' current influenza trend.\cr
#'
#' @name memtrend
#'
#' @param i.flu An object of class \code{mem}.
#' @param i.type Type of confidence interval to calculate the trend thresholds.
#' @param i.level Level of confidence interval to calculate the trend thresholds.
#' @param i.type.boot Type of bootstrap technique.
#' @param i.iter.boot Number of bootstrap iterations.
#'
#' @return
#' \code{memtrend} returns a list with two objects, the first one is the parameter used in
#' the calculations (\code{param.seasons}) and the second one (\code{trend.thresholds}) is
#' a matrix 1x2 with the Ascending (Delta) and Descending parameters (Eta).
#' \enumerate{
#' \item Delta - Ascending parameter.
#' \item Eta - Descending parameter.
#' }
#'
#' @details
#' This method is based on the Moving Epidemics Method (MEM) used to monitor influenza
#' activity in a weekly surveillance system.
#'
#' Input data is a data frame containing rates that represent historical influenza surveillance
#' data. It can start and end at any given week (tipically at week 40th), and rates can be
#' expressed as per 100,000 inhabitants (or per consultations, if population is not
#' available) or any other scale.
#'
#' The \code{i.seasons} parameter indicates how many seasons are used for calculating
#' thresholds. A value of -1 indicates the program to use as many as possible. If there
#' are less than this parameter, the program used all seasons avalaible.
#'
#' There are three different states for trend, to determine the state, the current rate
#' and the difference of the current and last weekly rate are needed:
#'
#' \itemize{
#' \item{2} {Ascending - When the weekly rate is above the epidemic threshold and
#' the difference of the current and last weekly rate is higher than Delta OR this is the
#' first time the rate is above the epidemic threshold.}
#' \item{3} {Descending - When the weekly rate is above the epidemic threshold
#' and the difference of the current and last weekly rate is lower than Eta OR this is the
#' first time the rate is below the epidemic threshold after having been above it.}
#' \item{1} {Stable - Otherwise.}
#' }
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # mem model
#' flucyl.mem<-memmodel(flucyl)
#' # Calculates trend thresholds
#' trend<-memtrend(flucyl.mem)
#' trend
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic - can we
#' detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281-3.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance
#' in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir
#' Viruses. 2013 Jul;7(4):546-58. DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in
#' Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other
#' Respir Viruses. 2015 Sep;9(5):234-46. DOI:10.1111/irv.12330.
#'
#' Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1];
#' Available from: \url{https://zenodo.org/record/165983}. DOI:10.5281/zenodo.165983
#'
#' @keywords influenza
#'
#' @export
memtrend<-function(i.flu,
                   i.type=1,
                   i.level=0.95,
                   i.type.boot="norm",
                   i.iter.boot=10000){
#   temporadas<-dim(i.data)[2]
#   if (i.seasons<1) i.seasons<-temporadas
#   temporadas.usar<-(max(temporadas-i.seasons+1,1)):temporadas
#   n.temp<-length(temporadas.usar)
#   datos<-i.data[temporadas.usar]
#   datos.2<-apply(datos,2,fill.missing)
#
#   datos<-i.flu$param.data
#   datos.2<-i.flu$data

  anios<-dim(i.flu$data)[2]
  semanas<-dim(i.flu$data)[1]
  datos.dif<-apply(i.flu$data,2,diff)
  datos.dif<-rbind(NA,datos.dif)
  rownames(datos.dif)[1]<-rownames(i.flu$data)[1]
  # datos.flu<-memmodel(datos,i.seasons,i.type.threshold,i.level.threshold,i.tails.threshold,i.type.intensity,i.level.intensity,i.tails.intensity,i.type.curve,i.level.curve,i.type.other,i.level.other,i.method,i.param,i.n.max,i.type.boot,i.iter.boot)
#   for (j in 1:anios){
#     # epidemic real start
#     i.i<-i.flu$seasons.data[1,j,1]-1
#     # epidemic real end
#     i.f<-i.flu$seasons.data[2,j,1]+2
#     if (i.i>=1) datos.dif[1:i.i,j]<-NA
#     if (i.f<=nrow(datos.dif)) datos.dif[i.f:nrow(datos.dif),j]<-NA
#   }
  for (j in 1:anios) datos.dif[-(i.flu$seasons.data[1,j,1]:min(semanas,1+i.flu$seasons.data[2,j,1])),j]<-NA
  datos.menos<-datos.dif
  datos.mas<-datos.dif
  datos.menos[datos.dif<0]<-NA
  datos.mas[datos.dif>0]<-NA
#   limite.s<-iconfianza.completo(as.numeric(datos.menos),tipo=1)$mayor[1]
#   limite.i<-iconfianza.completo(as.numeric(datos.mas),tipo=1)$menor[2]
  limite.s<-iconfianza(datos=as.numeric(datos.menos),nivel=i.level,tipo=i.type,ic=T,tipo.boot=i.type.boot,iteraciones.boot=i.iter.boot,colas=1)[1]
  limite.i<-iconfianza(datos=as.numeric(datos.mas),nivel=i.level,tipo=i.type,ic=T,tipo.boot=i.type.boot,iteraciones.boot=i.iter.boot,colas=1)[3]
  trend.thresholds<-matrix(c(limite.s,limite.i),ncol=2)
  colnames(trend.thresholds)<-c("Ascending Threshold","Descending Threshold")
  rownames(trend.thresholds)<-"Trend Thresholds"
  memtrend.output<-list(trend.thresholds=trend.thresholds,
                         param.flu=i.flu,
                         param.type=i.type,
                         param.level=i.level,
                         param.type.boot=i.type.boot,
                         param.iter.boot=i.iter.boot)
  memtrend.output$call<-match.call()
  return(memtrend.output)
}
