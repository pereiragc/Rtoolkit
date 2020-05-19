library(data.table)




setkey(DT, categ, val)

## ** Functions



dt_quantilebins_generic <- function(DT, numerical.var, weight.var, by.vars=NULL, outvarnames,
                                    wt.quantile.fun, ...) {
  # NOTE:  `...` are arguments to the cut function

  origkey <- key(DT)
  r <- DT[, {
    quantiles <- wt.quantile.fun(get(numerical.var), get(weight.var))
    # quantiles <- wt.quantile.fun(get(numerical.var))
    bin_label <- cut(get(numerical.var), breaks=quantiles, ordered_result = TRUE, ...)
    ll <- list(tmpvarname__ = get(numerical.var), bin_label=as.character(bin_label),
         bin_n=as.integer(bin_label), orig_pos=.I)
    names(ll)[1] <- numerical.var
    ll
  },by.vars]

    setkey(r, orig_pos)

  return(r)
}


##' Compute weighted quantiles
##'
##' Uses Hmisc::wtd.quantile to compute weighted quantiles.
##'
##' Use ntile=2 for median split; ntile=3 for terciles, ntile=4 for quartiles,
##' etc.
##'
##' the `...` are passed to the `cut` function.
dt_quantilebins_weighted <- function(DT, numerical.var, wt.var, by.vars=NULL, ntile=5, outvarnames=c("bin", "bin_n"),
                                     bounds=c(-Inf, Inf), ...) {

  prob_range <- seq(0, 1, by=1/ntile)
  bin_define <- prob_range[-c(1, length(prob_range))]

  quantile.fun <- function(x,w) {
    c(bounds[1], Hmisc::wtd.quantile(x, weights=w, bin_define), bounds[2])
  }

  dt_quantilebins_generic(DT, numerical.var, wt.var, by.vars, outvarnames, quantile.fun, ...)
}

