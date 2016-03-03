#' Plot Q10 values for groups and calculate optimum temperature.
#'
#' Plots and calcualtes the breakpoints for optimum temperature according to Q10 values. The default threshold is 1.9. Optimum temperature is assumed at intersection of of this threshold and the Q10 value curve for the respective group.  
#' @param x data matrix, numbers or NAs only, samples in lines, one column per measurement step.
#' @param groups group vector (must have same length as rows in data)
#' @param col color vector. Should have the same length as numberof groups. Otherwise grey shades are used.
#' @param marks add marks for the optimum temperature
#' @param error.bars if TRUE, standard error bars are added
#' @param ylim define own ylim
#' @param pch define dot style
#' @param legeng if TRUE, a legend is added
#' @param threshold defines the threshold for optimum temperature. Default is 1.9
#' @param plot.results plot results if TRUE (default). If FALSE, only calculated breakpoints are returned.
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @export
#' @examples
#' breakPointsQ10(x=data, group=groups, col=c("blue","red","black"))



breakPointsQ10 <- function(x, group, col="black", marks=T, error.bars=T, ylim=c(0,3), pch=16, legend=T, threshold=1.9,
                           plot.results=T, xlab="start point temperature", ylab="Q10 value") {
    q10.vals <- q10(x)
    m <- aggregate(q10.vals, by = list(group), mean, na.rm=T)
    e <- aggregate(q10.vals, by = list(group), sterr)
    
    #chech for sufficient colors:
    if(length(unique(group)) > length(col)){
        warning("not enough colors in vector col. Using greys instead")
        col <- paste0("grey", seq(0,70, length.out = length(unique(group))))
    }
    if (length(pch)<nrow(m)) {pch<-rep(pch, nrow(m))} #supply enough pch's for the loop
    
    if(plot.results){#initiallize plot (if desired)
        plot(as.numeric(m[1,-1]), ylim=ylim, type="b", pch=pch[1], col=col[1], xaxt="n", xlab=xlab, ylab=ylab, las=2)
        if (error.bars) {arrows(1:ncol(m), as.numeric(m[1,-1]-e[1,-1]), 1:ncol(m), as.numeric(m[1,-1]+e[1,-1]), angle = 90, length = 0.05, code = 3)}
        if (nrow(m)>1){
            for (i in 2:nrow(m)){
                points(as.numeric(m[i,-1]), type="b", pch=pch[i], col=col[i])
                if (error.bars) {arrows(1:ncol(m), as.numeric(m[i,-1]-e[i,-1]), 1:ncol(m), as.numeric(m[i,-1]+e[i,-1]), angle = 90, length = 0.05, code = 3)}
            }
        }
        labels <- gsub("X","",colnames(q10.vals),perl=T)
        axis(1, at=1:ncol(q10.vals), labels=labels)
        abline(h=threshold, lty=2)
    }
    #breakpoint calculation:
    th <- (m<threshold)[,-3:-1] #find values below threshold and skip the first three columns
    breakpoints <- rep(NA,nrow(m))
    for(i in 1:nrow(th)){
        for (ii in 1:ncol(th)){
            if(!is.na(th[i,ii]) & th[i,ii]==TRUE){ #find first TRUE value
                breakpoints[i] <-
                    (m[i,ii+2]-threshold)/((m[i,ii+2]-threshold)+(threshold-m[i,ii+3]))
                if(marks&plot.results){points(rep(breakpoints[i],2)+ii+1, c(-5,threshold), type="l", lty=2, col=col[i])}
                breakpoints[i] <- as.numeric(as.character(labels[ii+1])) + breakpoints[i]
                print(paste0(m$Group.1[i],": ", round(breakpoints[i],3)))
                break
            }
        }
    }
    if (legend) {legend("topright", legend=m$Group.1, pch=pch, col=col, lwd=1)} 
}

