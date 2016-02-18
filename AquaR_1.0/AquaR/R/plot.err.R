#' Plot means and error bars
#'
#' Plot the mean of a data vector, grouped be the group vector and add error bars.
#' @param data a numeric vector containing the data to plot.
#' @param groups vector containing the group information for data.
#' @param at optinal vector to arrange the position of the data points on the x-axis.
#' @param type type of plot: Either "dot" for dots, "bar" for bars or "diffbar" for bars, starting at the average.
#' @param barwidth if type="bar", barwidth defines the width of the bars.
#' @param col vector defining the color for each group. Must have the same length as number of groups.
#' @param err.bars error bars show standard error ("sterr") or standard deviation ("sd").
#' @param cex defines the size of the dots.
#' @param xlab label for x-axis.
#' @param ylab label for y-axis.
#' @param length defines the length of the arrow heads of the error bars
#' @param lwd line width of the error bars.
#' @param main title of the plot
#' @param las.x text orientation of the x-axis labels.
#' @param las.y text orientation of the y-axis labels.
#' @param xaxt add x-axis if TRUE
#' @param cex.yaxis scaling for y-axis labels
#' @param cex.xaxis scaling for x-axis labels
#' @param yscale vector of length two to scale the ylim's upper and lower limit
#' @param stats add ANOVA results to the plot
#' @param stats.mark add stars to significant ANOVA results
#' @param sig.groups vector with significant marker, which will be places over the error bars (order is 1:n and not according to at!)
#' @export
#' @examples
#' plot.err(data=c(1,2,3,5,3,2,4,12), groups = c(rep("a",4),rep("b",4)))





plot.err <- function(data, groups, at=FALSE, type="dot", barwidth=0.2, col="black", err.bars="sterr", cex=2, 
                     xlab="", ylab="means", length=0.1, lwd=1, main="", las.x=1, las.y=2, xaxt=T,
                     cex.yaxis=1, cex.xaxis=1, yscale=c(1,1), stats = T, stats.mark=T, sig.groups = F) {
  
  if(err.bars!="sterr" & err.bars!="sd"){stop("err.bars must be 'sd' (standard deviation) or 'sterr' (standard error)")} #check for correct err.bars setting
  
  means <- aggregate(data,list(groups),mean,na.rm=T) #calculate means
  if(err.bars=="sd"){errs<-aggregate(data,list(groups),sd,na.rm=T)}else{errs<-aggregate(data,list(groups),sterr)} #calculate values for error bars
  
  if(at[1]==F){at<-1:length(means[,1])} #define a standard 'at'
  if(length(at)!=length(means[,1])){stop("'at' must have same length as number of groups")} #check for correct 'at'
  
  if(type=="dot"){ #make dot chart:
    plot(at, means[,2], cex=cex, xaxt="n", xlab=xlab, pch=16, col=col,ylab=ylab,main=main,
         ylim=c(min(means[,2]-errs[,2]), max(means[,2]+errs[,2]))*yscale,
         xlim=c(0.5, length(means[,1])+0.5), las=las.y, cex.axis=cex.yaxis)
    arrows(at, means[,2]+errs[,2], at, means[,2]-errs[,2], code=3,length=length,angle=90,lwd=lwd, col=col)
  }
  if(type=="bar"){ #make bar chart:
    plot(at, means[,2], cex=cex, xaxt="n", xlab=xlab, pch=16, col="white",ylab=ylab,main=main,
         ylim=c(min(means[,2]-errs[,2]), max(means[,2]+errs[,2]))*yscale,
         xlim=c(0.5, length(means[,1])+0.5), las=las.y, cex.axis=cex.yaxis)
    for(i in 1:length(means[,1])){
      rect(at[i]-barwidth, 0, at[i]+barwidth, means[i,2], col=col[at[i]])
    }
    arrows(at, means[,2]+errs[,2], at, means[,2]-errs[,2], code=3,length=length,angle=90,lwd=lwd, col="black")
  }
  if(type=="diffbar"){ #make diff-bar chart:
    plot(at, means[,2], cex=cex, xaxt="n", xlab=xlab, pch=16, col="white",ylab=ylab,main=main,
         ylim=c(min(means[,2]-errs[,2]), max(means[,2]+errs[,2]))*yscale,
         xlim=c(0.5, length(means[,1])+0.5), las=las.y, cex.axis=cex.yaxis)
    for(i in 1:length(means[,1])){
      rect(at[i]-barwidth, mean(data, na.rm=T), at[i]+barwidth, means[i,2], col=col[at[i]])
    }
    arrows(at, means[,2]+errs[,2], at, means[,2]-errs[,2], code=3,length=length,angle=90,lwd=lwd, col="black")
  }
  
  if(xaxt==T){axis(1,labels=means[,1],at=at,las=las.x, cex.axis=cex.xaxis)}
  
  if(stats==T){# add ANOVA stats
    ao <- aov(data~as.factor(groups))
    pval <- summary(ao)[[1]][["Pr(>F)"]][1]
    if(stats.mark==T){
      if(pval<0.05){
        stars <- "*"
        if(pval<0.001){stars<-"***"}else{if(pval<0.01){stars<-"**"}}
        text(1, max(means[,2]+errs[,2])*yscale[2], labels=paste("ANOVA: ",round(pval,4),stars,sep=""), adj=c(0,1))
      }
      text(1, max(means[,2]+errs[,2])*yscale[2], labels=paste("ANOVA: ",round(pval,4),sep=""), adj=c(0,1))
    }
  }
  
  if(sig.groups[1]!=F){ #add sig.group markers
    ypos <- means[,2]+errs[,2]
    if(type=="diffbar"){ypos[which(ypos<mean(data, na.rm=T))] <- mean(data, na.rm=T) } #raise sig. markers over the mean line for diffbars
    text(at, ypos, labels=sig.groups[at], adj=c(NA,-0.5))
  } 
}




