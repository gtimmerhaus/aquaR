#' Plot heart rate data
#'
#' Specialized plot for temperature dependend maximum heart rate data. Every data point is plotted as a dot with standard error bars. Dots are connected by solid line but as soon as the number of samples represented by the dat gets below the cutoff, the connective line is dashed.
#' @param data data matrix, numbers or NAs only, samples in lines, one column per measurement step.
#' @param groups group vector (must have same length as rows in data)
#' @param xlabels labels for the x-axis
#' @param dodge add a dodge on xaxis for groups
#' @param sig.marks if TRUE (default) grey rectangle and asterisks are added to the background of the plot to mark significant differences in groups according to ANOVA (*: p<0.05, **: p<0.01) 
#' @param xlim define own xlim
#' @param ylim define own ylim
#' @param col color vector. If insufficient number of colors is given black to grey colors are used.
#' @param pch defines dot style. Can be a vector with length = number of groups
#' @param err.bar.col if TRUE (default), error bars will be drawn in same color as group colors. Otherwise in black
#' @param cex scaling of data points
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param length length of error bar heads
#' @param lwd line width of dot connectors
#' @param lwd.err line width of error bars
#' @param main title of plot
#' @param las.x text orientation for x-axis labels (1=horizontal, 2=vertical)
#' @param las.y text orientation for y-axis (2=horizontal, 1=vertical)
#' @param legend.pos position of legend 
#' @param legend if TRUE, a legend will be added to the plot 
#' @param at define order of the groups and positioning for the legend labels
#' @param bg.grid if TRUE, a background grid will be drawn
#' @param cutoff percentage of samples needed for draing a solid line, less samples will be drawn with a dashed line.
#' @param leg.labels vector containing the labels for the legend
#' @export
#' @examples
#' HR.plot(hr, dat$group, col=c("blue","red"))



HR.plot <- function(data, groups, xlabels=colnames(data), dodge=0.03, sig.marks=T, 
                    xlim="", ylim="", cex=2, col="", pch=16, err.bar.col = T,
                    xlab="degrees C", ylab="maximum heart rate (bpm)", 
                    length=0.02, lwd=1, lwd.err=1, main="", las.x=1,las.y=2, 
                    legend.pos="bottomright", legend=TRUE, bg.grid=T, at="", 
                    cutoff=0.5, leg.labels=""){
  
  #calculate means and standard errors for the data:
  means <- aggregate(data, list(groups), mean, na.rm=T)  
  errs <- aggregate(data, list(groups), sterr)
  n <- length(means[,1]) #number of groups
  x<- 2:length(means[1,]) 
  if (ylim[1]==""){ylim= c(min(means[,-1]-errs[,-1], na.rm=T), max(means[,-1]+errs[,-1], na.rm=T))}
  if (xlim[1]==""){xlim= c(2,length(means[1,]))}
  
  #chech for sufficient colors:
  if(n > length(col)){
      warning("not enough colors in vector col. Using greys instead")
      col <- paste0("grey", seq(0,70, length.out = n))
  }
  
  if (at[1]==""){at=1:n}
  #rearange data according to "at":
  means <- means[at,]
  errs <- errs[at,]
  
  plot(x, x, col="white", xlim=xlim, ylim=ylim, las=las.y, ylab=ylab, xaxt="n", xlab=xlab)
  
  #significance markers:
  if(sig.marks == T){
      for (i in 1:ncol(data)){
          #get ANOVA value for each column:
          p.val <- summary(aov(data[,i]~groups))[[1]][["Pr(>F)"]][1]
          if(p.val <0.01){
              rect(i+0.5,0,i+1.5,500, col = "grey90", border = NA)
              text(i+1, ylim[2], labels="**")
          }else{
              if(p.val <0.05){
                  rect(i+0.5,0,i+1.5,500, col = "grey95", border = NA)
                  text(i+1, ylim[2], labels="*")
              }
          }
          
      }
  }
  #background grid:
  if(bg.grid == T){
    abline(h=1:20*10, v=1:100, col="grey",lty=3)
  }
  axis(1, at=x, labels=xlabels, cex.axis=0.8)
  
  #error bars
  if (err.bar.col == T) {col.bar <- col}else{col.bar <- rep("black",n)}
  for (i in 1:n) {
    arrows(x+(i-round(n/2))*dodge,as.numeric(means[i,-1])-as.numeric(errs[i,-1]),
           x+(i-round(n/2))*dodge,as.numeric(means[i,-1])+as.numeric(errs[i,-1]),
           angle=90, length=length, code=3, lwd=lwd.err, col = col.bar[i])  
  }
  #estimate plotting region for n<0.5
  count_nas <- function(arg1) {return(sum(is.na(arg1)))}
  len <- aggregate(data, list(groups), count_nas)
  solid <- len[,-1]<=n*cutoff # mark TPs with less than half samples as FLASE
  temp <- means[,-1] # initialize temp
  temp[means[,-1]*solid==0] <- NA #remove marked measures
  #plot solid line:
  for (i in 1:n) {
    points(x+(i-round(n/2))*dodge,temp[i,], type="b", col=col[i], lwd=lwd, pch=pch)
  }
  #mark the last TRUE as FALSE in each line (to have a connecting line to the solid lines):
  for (i in 1:n){solid[i,table(solid[i,])["TRUE"] ] <- FALSE}
  #Draw the dashed line for the remaining:
  temp <- means[,-1] 
  temp[means[,-1]*(!solid)==0] <- NA 
  #plot dashed line:
  for (i in 1:n) {
    points(x+(i-round(n/2))*dodge,temp[i,], type="b", col=col[i], lwd=lwd, pch=pch, lty=2)
  }
  #legend:
  if(leg.labels[1]==""){leg.labels<-means[,1]}
  if (legend==T){legend(legend.pos, legend=leg.labels, col=col, lwd=1, pch=16)}
}

