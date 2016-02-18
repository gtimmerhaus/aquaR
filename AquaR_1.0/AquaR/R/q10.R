#' Calculate Q10 values column-wise.
#'
#' Calculates Q10 values for an input matrix column-wise according to the formula (R2/R1)^10 with R1 and R2 being consecutive measurements. 
#' @param x numeric matrix containing the consecutive measurements in columns and test subjects in rows.
#' @export
#' @examples
#' q10(matrix(11:20, ncol=5))

q10 <-  function(x){  
  n <- colnames(x)[-ncol(x)]
  x <- (x[,-1]/x[,-ncol(x)])^10
  colnames(x) <- n
  return(x)
}


