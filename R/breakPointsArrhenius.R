#' produce an Arrhenius plot for groups and calculate optimum temperature.
#'
#' Plots and calcualtes the breakpoints for optimum temperature according to the Arrhenius method. The function censecutively adds measurements and calculates the correlation coefficient. If the coefficient is not increasing anymore it assumes that the linear phase ended and calculates the break point. 
#' @param x data matrix, numbers or NAs only, samples in lines, one column per measurement step.
#' @param groups group vector (must have same length as rows in data)
#' @param col color vector. Should have the same length as number of groups. Otherwise grey shades are used.
#' @param marks add marks for the optimum temperature
#' @param obs minimum number of observations to calculate the correlation in the linear phase. Default is 3. 
#' @param error.bars if TRUE, standard error bars are added
#' @param ylim define own ylim
#' @param pch define dot style
#' @param legeng if TRUE, a legend is added
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @export
#' @examples
#' breakPointsArrhenius(x=data[,-1], group=groups)

x<- normalized_w10[,11:18]
group <- normalized_w10$group
col <- cols[c(1,5,9)]


breakPointsArrhenius <- function(x, group, col="black", marks=T, obs=3, error.bars=T,
                                 ylim=c(4,5.2), pch=16, legend=T, xlab="Temperature (degrees C)", ylab="ln(HR max)") {
    arr.vals <- rev(log(x))
    m <- aggregate(arr.vals, by = list(group), mean, na.rm=T)
    e <- aggregate(arr.vals, by = list(group), sterr)
    
    if(obs<3){stop("obs needs to be bigger than 2")} #correlation with two or less values not meaninful
    
    #chech for sufficient colors:
    if(length(unique(group)) > length(col)){
        warning("not enough colors in vector col. Using greys instead")
        col <- paste0("grey", seq(0,70, length.out = length(unique(group))))
    }
    if (length(pch)<nrow(m)) {pch<-rep(pch, nrow(m))} #supply enough pch's for the loop
    
    labels <- gsub("X","",colnames(arr.vals),perl=T)
    
    plot(as.numeric(m[1,-1]), ylim=ylim, type="p", pch=pch[1], 
         col=col[1], xaxt="n", xlab=xlab, ylab=ylab, las=2)
    if (error.bars) {arrows(1:ncol(m), as.numeric(m[1,-1]-e[1,-1]), 1:ncol(m), as.numeric(m[1,-1]+e[1,-1]), angle = 90, length = 0.05, code = 3)}
    if (nrow(m)>1){
        for (i in 2:nrow(m)){
            points(as.numeric(m[i,-1]), type="p", pch=pch[i], col=col[i])
            if (error.bars) {arrows(1:ncol(m), as.numeric(m[i,-1]-e[i,-1]), 1:ncol(m), as.numeric(m[i,-1]+e[i,-1]), angle = 90, length = 0.05, code = 3)}
        }
    }
    axis(1, at=1:ncol(arr.vals), labels=labels)
    
    #breakpoint calculation:
    res <- matrix(rep(NA,nrow(m)*2),ncol=2)
    colnames(res) <- c("bestCor", "column")
    temp <- m[,-1] #remove group column
    for(i in 1:nrow(temp)){ #one iteration per group
        bestCor <- 1 #init a bad correlation value (target will be close to -1)
        for(ii in (ncol(temp)-obs+1):2){ #find the best correlation:
            if(cor(as.numeric(temp[i,ii:ncol(temp)]), ii:ncol(temp), use="complete.obs") < bestCor){#find the best correlation from last point to ii
                bestCor <- cor(as.numeric(temp[i,ii:ncol(temp)]), ii:ncol(temp), use="complete.obs")
                res[i,1] <- bestCor #save best correlation
                res[i,2] <- ii  #remember column with best correlation
            }
        }
        #add linear models for best correlation:
        linInc <- lm(as.numeric(temp[i,ncol(temp):res[i,2]])~c(ncol(temp):res[i,2])) #lm() for linear increase
        beyInc <- lm(as.numeric(temp[i,(res[i,2]-1):1])~c((res[i,2]-1):1)) #lm() for beyond breakpoint observations
        #calculate meeting point:
        xpos <- (beyInc$coefficients[1]-linInc$coefficients[1])/(linInc$coefficients[2]-beyInc$coefficients[2])
        ypos <- linInc$coefficients[1]+linInc$coefficients[2]*xpos
        #draw linear lines:
        points(c(xpos, ncol(temp)), c(ypos, linInc$coefficients[1]+linInc$coefficients[2]*ncol(temp)), col=col[i], type="l")
        points(c(xpos, 1), c(ypos, beyInc$coefficients[1]+beyInc$coefficients[2]*1), col=col[i], type="l")
        #draw indicator line:
        points(rep(xpos,2), c(ypos,0), type="l", lty=2, col=col[i])
        #print calculated ABT:
        print(paste0(m$Group.1[i],": ", round(as.numeric(labels[floor(xpos)]) - xpos %% 1,3)))
    }
    
    if (legend) {legend("topright", legend=m$Group.1, pch=pch, col=col, lwd=1)}
}


