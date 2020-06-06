
lexicompare  <- function(..., vals, binop = `<=`){
  ## Row-wise lexicographic comparison
  ## For use within data.table.
  ##
  ## Example:
  ## DT <- data.table(c1 = c(3,3,1,3,4), c2 = c(3,2,2,1,1), c3 = c(1,1,-2,3,5))
  ## vals <- c(3,2,1)
  ## DT[, cmp := lexicompare(c1,c2,c3, vals=vals)]

  lcols  <- list(...)

  if (length(lcols) != length(vals))
    stop("[lexicompare] Either too many or too few values.")

  vec.result  <- tie  <- rep(TRUE, length(lcols[[1]]))
  retteb  <- better  <- rep(NA, length(lcols[[1]]))

  for (i in 1:length(lcols)){
    better[tie] <- binop(lcols[[i]][tie], vals[i])
    retteb[tie]  <- binop(vals[i], lcols[[i]][tie])
    tie  <- better & retteb

    vec.result[!tie]  <- better[!tie]

    if(!any(tie)){
      break
    }
  }

  return(vec.result)
}
