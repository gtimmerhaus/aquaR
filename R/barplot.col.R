#' Barplot with error bars
#'
#' Returns a barplot based on a given data.frame. The mean value of each column will be calculated and used for the plot. Error bars will be added, using standard error (default) or standard deviation.
#' @param data a data.frame. Non-numeric columns will be removed.
#' @param lwd line width of the error bars
#' @param length length of error bar heads
#' @param err error bars show standard error (ster) or standard deviation (sd)
#' @export barplot.col
#' @examples
#' x <- c(2,4,3,5,NA,5)
#' sterr(x)

barplot.col <- function(data,lwd=1,length=0.2, err="ster",col="grey",las=1) {
  ifelse(err=="ster" | err=="sd",1, stop("'err=' must be 'ster' (standard error) or 'sd' (standard deviation)", call. = FALSE))
  
  #remove non-numeric data from data.frame:
  cols <- NULL
  for (i in 1:length(data)) {
    if(class(data[,i])=="numeric" | class(data[,i])=="integer") {cols <- c(cols,i)}
  }
  if(length(cols)==0) {stop("data must contain numerical data", call.=FALSE)}
  dat <- data[,cols]
  
  #calculate values
  x <- barplot(apply(dat,2,mean,na.rm=T))
  ifelse(err=="ster", ste <- apply(dat,2,sterr), ste <- apply(dat,2,sd,na.rm=T))
  means <- apply(dat,2,mean,na.rm=T)
  
  #plot
  barplot(apply(dat,2,mean,na.rm=T), ylim=c(min(c(0,means-ste),na.rm=T),max(c(0,means+ste),na.rm=T)), col=col,las=las)
  arrows(x, means+ste, x, means-ste,length = length,angle = 90,code=3,lwd=lwd)
} 

