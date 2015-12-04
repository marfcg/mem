#' Goodness of fit of the mem
#'
#' Function \code{memgoodness} perform the goodness of fit of the mem
#'
#' To be written
#'
#' @name memmodel
#'
#' @param ... same as \code{memmodel}
#' @param i.calculation.method method of determining true/false positives and true/false negatives.
#' @param i.goodness.method method to calculate goodness.
#' @param i.detection.values values to use in the i.param value of \code{memtiming}.
#' @param i.weeks.above number of weeks over the threshold to give the alert.
#' @param i.output output directory for graphs.
#' @param i.graph whether the graphs must be written or not.
#' @param i.prefix prefix used for naming graphs.
#'
#' @return
#' \code{memgoodness} returns a list.
#' An object of class \code{mem} is a list containing at least the following components:
#'   \item{validity.data}{data for each value analysed.}
#'   \item{results}{Total weeks, non-missing weeks, true positives, false positives
#' true negatives, false negatives, sensitivity, specificity .}
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucyl)
#' # Finds the timing of the first season: 2001/2002
#' gepi<-memgoodness(flucyl,i.detection.values=seq(2.5,2.8,0.1))
#' gepi
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
#'
#' @keywords influenza
#'
#' @export
memgoodness<-function(i.data,
                      i.seasons=10,
                      i.type.threshold=5,
                      i.level.threshold=0.95,
                      i.tails.threshold=1,
                      i.type.intensity=6,
                      i.level.intensity=c(0.40,0.90,0.975),
                      i.tails.intensity=1,
                      i.type.curve=2,
                      i.level.curve=0.95,
                      i.type.other=2,
                      i.level.other=0.95,
                      i.method=2,
                      i.param=2.8,
                      i.n.max=-1,
                      i.type.boot="norm",
                      i.iter.boot=10000,
                      i.calculation.method="default",
                      i.goodness.method="cross",
                      i.detection.values=seq(2.0,3.0,0.1),
                      i.weeks.above=1,
                      i.output=".",
                      i.graph=F,
                      i.prefix=""){

  anios<-dim(i.data)[2]
  semanas<-dim(i.data)[1]
  validacion<-array(dim=c(8,anios),dimnames=c("year","indicator"))
  colnames(validacion)<-names(i.data)

  if (!(i.goodness.method=="sequential")){
    # Metodo 2: cruzada
    if (anios>5){
      for (i in 1:anios){
        indices.2<-(1:anios)-i
        indices.1<-abs(indices.2)
        indices.modelo<-order(indices.1,indices.2)[2:11]
        indices.modelo<-sort(indices.modelo[!is.na(indices.modelo)])
        indices.actual<-i
        datos.actual<-i.data[indices.actual]
        datos.modelo<-memmodel(i.data[indices.modelo],
                                i.seasons,
                                i.type.threshold,
                                i.level.threshold,
                                i.tails.threshold,
                                i.type.intensity,
                                i.level.intensity,
                                i.tails.intensity,
                                i.type.curve,
                                i.level.curve,
                                i.type.other,
                                i.level.other,
                                i.method,
                                i.param,
                                i.n.max,
                                i.type.boot,
                                i.iter.boot)
        validacion.i<-calcular.indicadores(i.current=datos.actual,
                                           i.umbral.pre=datos.modelo$pre.post.intervals[1,3],
                                           i.umbral.pos=datos.modelo$pre.post.intervals[2,3],
                                           i.intensidades=datos.modelo$epi.intervals[,4],
                                           i.duracion.intensidad=datos.modelo$mean.length,
                                           i.metodo.calculo=i.calculation.method,
                                           i.semanas.por.encima=i.weeks.above,
                                           i.valores.parametro.deteccion=i.detection.values,
                                           i.output=i.output,
                                           i.graph=i.graph,
                                           i.graph.name=paste(i.prefix,"[mem] Cross ",i,sep=""))
        validacion[,i]<-validacion.i$indicadores.t
        rownames(validacion)<-rownames(validacion.i$indicadores.t)
      }
    }
  }else{
    # Metodo 1: secuencial
    if (anios>5){
      for (i in 6:anios){
        indices.modelo<-max(1,i-10):(i-1)
        indices.actual<-i
        datos.actual<-i.data[indices.actual]
        datos.modelo<-memmodel(i.data[indices.modelo],
                               i.seasons,
                               i.type.threshold,
                               i.level.threshold,
                               i.tails.threshold,
                               i.type.intensity,
                               i.level.intensity,
                               i.tails.intensity,
                               i.type.curve,
                               i.level.curve,
                               i.type.other,
                               i.level.other,
                               i.method,
                               i.param,
                               i.n.max,
                               i.type.boot,
                               i.iter.boot)
        validacion.i<-calcular.indicadores(i.current=datos.actual,
                                           i.umbral.pre=datos.modelo$pre.post.intervals[1,3],
                                           i.umbral.pos=datos.modelo$pre.post.intervals[2,3],
                                           i.intensidades=datos.modelo$epi.intervals[,4],
                                           i.duracion.intensidad=datos.modelo$mean.length,
                                           i.metodo.calculo=i.calculation.method,
                                           i.semanas.por.encima=i.weeks.above,
                                           i.valores.parametro.deteccion=i.detection.values,
                                           i.output=i.output,
                                           i.graph=i.graph,
                                           i.graph.name=paste(i.prefix,"[mem] Seque ",i,sep=""))
        validacion[,i]<-validacion.i$indicadores.t
        rownames(validacion)<-rownames(validacion.i$indicadores.t)
      }
    }
  }

  resultado<-apply(validacion,1,sum,na.rm=T)
  resultado[7]<-resultado[3]/(resultado[3]+resultado[6])
  resultado[8]<-resultado[5]/(resultado[5]+resultado[4])

  memgoodness.output<-list(validity.data=validacion,
                          results=resultado,
                          param.data=i.data,
                          param.seasons=i.seasons,
                          param.type.threshold=i.type.threshold,
                          param.level.threshold=i.level.threshold,
                          param.tails.threshold=i.tails.threshold,
                          param.type.intensity=i.type.intensity,
                          param.level.intensity=i.level.intensity,
                          param.tails.intensity=i.tails.intensity,
                          param.type.curve=i.type.curve,
                          param.level.curve=i.level.curve,
                          param.type.other=i.type.other,
                          param.level.other=i.level.other,
                          param.method=i.method,
                          param.param=i.param,
                          param.n.max=i.n.max,
                          param.type.boot=i.type.boot,
                          param.iter.boot=i.iter.boot,
                          param.calculation.method=i.calculation.method,
                          param.goodness.method=i.goodness.method,
                          param.detection.values=i.detection.values,
                          param.weeks.above=i.weeks.above,
                          param.output=i.output,
                          param.graph=i.graph,
                          param.prefix=i.prefix)
  memgoodness.output$call<-match.call()
  return(memgoodness.output)
}
