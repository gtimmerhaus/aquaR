#' Plot means and error in group and subgroup
#'
#' Plot group data in the background and subgroups in the foreground with mean+/-sterr
#' @param data a numeric vector containing the data to plot.
#' @param group1 vector containing the (background) group information for data.
#' @param group2 vector containing the sub-group information for data.
#' @param rect.width width of the rectangle of group1 
#' @param at optional vector to arrange the position of the data points on the x-axis. Must be integer.
#' @param colG1 vector defining the colors for group1. Must have the same length as number of groups.
#' @param colG2 vector defining the colors for group2. Must have the same length as number of groups.
#' @param shading intensity (between 1 and 99) of overlay of the background (group1) with a transparent white layer (default FALSE)
#' @param lwd line width of mean marker (group1) and the error bars (group2).
#' @param err.bars error bars show standard error ("sterr") or standard deviation ("sd").
#' @param cex defines the size of the dots (means of group2).
#' @param xlab label for x-axis.
#' @param ylab label for y-axis.
#' @param length defines the length of the arrow heads of the error bars
#' @param legend.pos postion of legend (for group2)
#' @param main title of the plot
#' @param las.x text orientation of the x-axis labels.
#' @param las.y text orientation of the y-axis labels.
#' @param xaxt add x-axis if TRUE
#' @param cex.yaxis scaling for y-axis labels
#' @param cex.xaxis scaling for x-axis labels
#' @param yscale vector of length two to scale the ylim's upper and lower limit
#' @export plot.err.two.factor
#' @examples
#' plot.err.two.factor(dat$V, dat$Gr.navn, dat$feed, colG1=cols[3:1], colG2 = cols[6:4], lwd=3, shading=30, at=c(3,2,1))


plot.err.two.factor <- function(data, group1, group2, rect.width = c(-0.3,0.3), at=FALSE, 
                                colG1="grey", colG2="black", err.bars="sterr", cex=2, shading = FALSE,
                                xlab="", ylab="means", length=0.1, lwd=1, main="", las.x=1, las.y=2, 
                                xaxt=T, legend.pos="topright", cex.yaxis=1, cex.xaxis=1, yscale=c(1,1)) {
    
    if(err.bars!="sterr" & err.bars!="sd"){stop("err.bars must be 'sd' (standard deviation) or 'sterr' (standard error)")} #check for correct err.bars setting
    
    meansG1 <- aggregate(data,list(group1),mean,na.rm=T) #calculate means
    if(err.bars=="sd"){errsG1<-aggregate(data,list(group1),sd,na.rm=T)}else{errsG1<-aggregate(data,list(group1),sterr)} #calculate values for error bars
    
    if(at[1]==F){at<-1:length(meansG1[,1])} #define a standard 'at'
    if(length(at)!=length(meansG1[,1])){stop("'at' must have same length as number of groups")} #check for correct 'at'
    
    meansG2 <- aggregate(data,list(group1,group2),mean,na.rm=T) #calculate means
    if(err.bars=="sd"){errsG2<-aggregate(data,list(group1,group2),sd,na.rm=T)}else{errsG2<-aggregate(data,list(group1,group2),sterr)} #calculate values for error bars

    if(length(colG1) < nrow(meansG1)){colG1 <- rep(colG1, nrow(meansG1));warning("colG1 shorter than n of group1")}
    if(length(colG2) < length(levels(meansG2[,2]))){colG2 <- rep(colG2, length(levels(meansG2[,2])));warning("colG2 shorter than n of group2")}
       
    #prepare plot:
    plot(at, meansG1[,2], cex=cex, xaxt="n", xlab=xlab, pch=16, col=col,ylab=ylab,main=main,
         ylim=c(min(meansG2[,3]-errsG2[,3]), max(meansG2[,3]+errsG2[,3]))*yscale,
         xlim=c(0.5, length(meansG1[,1])+0.5), las=las.y, cex.axis=cex.yaxis)
    #add boxes for group1 means and errors:
    rect(at+rect.width[1], meansG1[,2]+errsG1[,2], at+rect.width[2], meansG1[,2]-errsG1[,2], col=colG1)
    for(i in 1:nrow(meansG1)){
        points(c(at[i]+rect.width[1], at[i]-rect.width[1]), rep(meansG1[i,2],2), type="l", lwd=lwd)
    }
    
    #add shading:
    if(shading>0 & shading<100){
        rect(min(at+rect.width[1])*0.9, max(meansG1[,2]+errsG1[,2])*1.1, 
             max(at+rect.width[2])*1.1, min(meansG1[,2]-errsG1[,2])*0.9, col=paste0("#FFFFFF", shading), border = F)
    }
    
    #add means and error bars for group2:
    x <- seq(rect.width[1], rect.width[2], length.out = nrow(meansG2)/nrow(meansG1)+2)
    for(i in 1: nrow(meansG1)){
        y <- subset(meansG2[,3], meansG2$Group.1==meansG1[i,1])
        y.err <- subset(errsG2[,3], errsG2$Group.1==meansG1[i,1])
        arrows(x + at[i], c(NA, y+y.err ,NA), x + at[i], c(NA, y-y.err ,NA), code=3,length=length,angle=90,lwd=lwd, col=c(NA,colG2,NA))
        points(x + at[i], c(NA, y ,NA), cex = cex, pch=16, col=c(NA,colG2,NA))
    }

    if(xaxt==T){axis(1,labels=meansG1[,1],at=at,las=las.x, cex.axis=cex.xaxis)}
    
    legend(legend.pos, pch=16, legend=levels(meansG2[,2]), col=colG2)
    
}




