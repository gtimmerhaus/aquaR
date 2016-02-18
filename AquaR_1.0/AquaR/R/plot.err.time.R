#' Plot timeline data for multiple groups 
#'
#' Calculates the mean value for each group and time point. Adds error bars, showing +/- standard error (default) or standard deviation. Data is plotted with dots and lines.
#' @param data a numeric vector containing the data to plot.
#' @param time a vector containing the time point information for data.
#' @param groups vector containing the group information for data.
#' @param col vector defining the color for each group. Must have the same length as number of groups.
#' @param err error bars show standard error ("sterr") or standard deviation ("sd").
#' @param dodge adds a dodge to the x-axis position of the groups.
#' @param cex defines the size of the dots.
#' @param length defines the length of the arrow heads of the error bars
#' @param legend.pos defines the legend position. Must be "topleft", "topright", "bottomleft" or "bottomright". If legend != TRUE, no legend will be drawn.
#' @param stats adds markers for significant differences (ANOVA, for the respective timepoint) to the bottom of the plot.
#' @export
#' @examples
#' plot.err.time(data = c(1,2,4,5,4,3,2,3,3,4,6,5,4,3,2,3,4,5,2,3), time = c(rep(c(rep(1,5),rep(2,5)),2)), groups = c(rep("a",10),rep("b",10)), col=c("blue","red"))

plot.err.time <- function(data, time, groups, col="black", err.bars="sterr", dodge=0.02, cex=2, xlab="", ylab="means", length=0.02, lwd=1, main="", las.x=1,las.y=2, legend.pos="topleft", legend=TRUE, ylim=NULL, stats=FALSE) {
  
  if(err.bars!="sterr" & err.bars!="sd"){stop("err.bars must be 'sd' (standard deviation) or 'sterr' (standard error)")} #check for correct err.bars setting
 
  means <- aggregate(data~time+groups, FUN = mean, na.rm=T) #calculate means
  if(length(col)!=length(levels(as.factor(means[,2])))){stop("'col' vector must have same length as number of groups")} #check for a correct color vector
  
  if(err.bars=="sd"){errs<-aggregate(data~time+groups, FUN = sd, na.rm=T)}else{
    errs<-aggregate(data~time+groups, FUN = sterr)} #calculate values for error bars
  
  if(is.null(ylim)){ylim=c(min(means[,3]-errs[,3]), max(means[,3]+errs[,3]))} #calculate ylim based on max/min means and errs
  x <- 1:length(levels(as.factor(means[,1]))) #define 'x' based on time points
  
  plot(x, x, xaxt="n", xlab=xlab, col=NULL, ylab=ylab,main=main,
       xlim=c(0.8, length(x)+0.2), ylim=ylim, las=las.y
  )
  axis(1,labels=levels(as.factor(means[,1])),at=x,las=las.x)
  
  ngroup <- length(levels(as.factor(means[,2]))) #number of groups
  dodge <- seq(0,2*dodge,length.out = ngroup) #calculate dodge for the groups
  dodge <- dodge-mean(dodge) #adjust dodge to the center
  
  ii <- 0
  for (i in levels(as.factor(means[,2]))) { #one iteration per group
    ii<- ii+1
    temp <- means[means[,2]==i,3] #y data
    points(x+dodge[ii], temp, type="b", pch=16, col=col[ii], lwd=lwd)
    tempErr <- errs[errs[,2]==i,3] #error bars range
    arrows(x+dodge[ii], temp+tempErr, x+dodge[ii], temp-tempErr, code=3,length=length,angle=90,lwd=lwd, col=col[ii])
  }
  if(stats==TRUE){ #run an ANOVA per time point and mark the significance 
    ii <- 0
    for(i in levels(as.factor(means[,1]))){
      ii <- ii + 1
      p <- summary(aov(data[time==i]~groups[time==i]))[[1]][["Pr(>F)"]][[1]]
      pt <- " "
      if(p<0.1){pt<-"."}
      if(p<0.05){pt<-"*"}
      if(p<0.01){pt<-"**"}
      if(p<0.001){pt<-"***"}
      if(pt!=" "){text(x[ii],ylim[1], labels = pt)}
    }
  }
  if(legend==TRUE){
    legend(legend.pos, legend=levels(as.factor(means[,2])), col=col, lwd=lwd,pch=16, bty = "n", cex=0.8, pt.cex = 1.1)
  }
}


