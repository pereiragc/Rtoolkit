## library(devtools)
use_package("data.table")
use_package("Hmisc")

## ** Functions

##' Generic function for quantile binning
##'
##' Creates quantiles of variable `numerical.var` (supplied as a single string)
##' using weights given by `weight.var`. 
##'
##' @title Generic quantile binning
##' @param DT Data table to be summarized
##' @param numerical.var [str(1)] which variable will be quantilized
##' @param weight.var [num] weights with which quantiles are constructed
##' @param by.vars [str(n)] vector of variables by which
##' @param outvarnames not implemented yet: control return names
##' @param wt.quantile.fun [function] f(x,w) where x and w have equal length
##' @param ... extra arguments to `cut`
##' @return
##' @author Gustavo
dt_quantilebins_generic <- function(DT, numerical.var, weight.var, by.vars=NULL, outvarnames,
                                    wt.quantile.fun, ...) {
  r <- DT[, {
    quantiles <- wt.quantile.fun(get(numerical.var), get(weight.var))
    # quantiles <- wt.quantile.fun(get(numerical.var))
    bin_label <- cut(get(numerical.var), breaks=quantiles, ordered_result = TRUE, ...)
    ll <- list(tmpvarname__ = get(numerical.var), bin_label=as.character(bin_label),
         bin_n=as.integer(bin_label), orig_pos=.I)
    names(ll)[1] <- numerical.var
    ll
  },by.vars]

  data.table::setkey(r, orig_pos)
  r[, orig_pos := NULL]

  return(r)
}


##'  Compute weighted quantiles of a data.table `DT`
##'
##'  Apply `dt_quantilebins_generic` with Hmisc's `wtd.quantile` to find
##'  weighted quantiles. The number of percentiles is governed by `ntile`: with
##'  `ntile=5`, the function computes quintiles, with `ntile=4` quartiles,and so
##'  on.
##'
##' The `...` are passed to the `cut` function. Note that `ordered_result=TRUE`
##' necessarily (i.e., it's not an option).
##'
##'
##' @title Weighted quantile bins from data.table data
##' @param DT [data.table] data
##' @param numerical.var [str(1)] variable to be quantilized
##' @param wt.var [str(1)] variable to be used as weight
##' @param by.vars [str(n)] variable to subset by when computing quantiles
##' @param ntile [int] number of quantiles
##' @param outvarnames to be implemented
##' @param bounds [num(2)] natural bounds for `numerical.var` (for pretty printing)
##' @param ... extra arguments to the `cut` function
##' @return
##' @author Gustavo
##' @export
dt_quantilebins_weighted <- function(DT, numerical.var, wt.var, by.vars=NULL, ntile=5, outvarnames=c("bin", "bin_n"),
                                     bounds=c(-Inf, Inf), ...) {

  prob_range <- seq(0, 1, by=1/ntile)
  bin_define <- prob_range[-c(1, length(prob_range))]

  quantile.fun <- function(x,w) {
    c(bounds[1], Hmisc::wtd.quantile(x, weights=w, bin_define), bounds[2])
  }

  dt_quantilebins_generic(DT, numerical.var, wt.var, by.vars, outvarnames, quantile.fun, ...)
}


#' @export
set_group <- function(DT, col_group, group_list, groupvarname = NULL, ...) {
  ## Format of `group_list`:
  ## list(
  ##   group1 = c("g1_level1", "g1_level2", ...)
  ##   group2 = c("g2_level1", "g2_level2", ...)
  ## )

  ## Ellipsis are passed to the factor command


  ## Create dictionary:
  dict <- data.table::rbindlist(lapply(names(group_list), function(gname) {
    data.table(groupname = gname,
               vals = group_list[[gname]])
  }))
  # dict[, levels := factor(levels, levels=levels, ...)]
  dict[, groupname := factor(groupname, levels=unique(groupname))]



  if (is.null(groupvarname)) {
    groupvarname <- paste(col_group, "group", sep="_")
  }

  # setnames(dict, c("groupname", "vals"), c(groupvarname, col_group))
  data.table::setnames(dict, c("vals"), c(col_group))

  DT[dict, (groupvarname) := groupname, on=col_group]
}

#' @export
tab <- function(DT, cols_tab, cols_subs=NULL, percent=TRUE) {
  mm <- ifelse(percent, 100, 1)

  tot <- DT[, .N]

  # All the necessary computation is done here...
  res <- DT[, .(`Number of occurrences` = .N), c(cols_tab, cols_subs)]
  res[, `Proportion of total` := mm*`Number of occurrences`/ tot]
  res[, `Proportion in group` := mm*`Number of occurrences`/ sum(`Number of occurrences`), cols_subs]
  # ... the rest is just taking this results and outputting them

  subs_gr <- unique(res[, cols_subs, cols_subs, with=FALSE]) # Find unique subsetting groups

  data.table::setkeyv(res, cols_subs)
  data.table::setkeyv(subs_gr, cols_subs)

  ## Loop over subsetting groups
  if (nrow(subs_gr) == 0) {
    return(list(list(
      group_name = "Full sample",
      tab = res[order(get(cols_tab))]
    )))
  }

  subs_names <- names(subs_gr) # Keep subsetting group names
  lapply(1:nrow(subs_gr),
         function(i) {
           ## For subset group `i`, lookup values in `res`
           this_gr <- subs_gr[i]
           tab <- this_gr[res, nomatch=0]
           group_vals <- paste(subs_names, unlist(this_gr), sep=" = ")

           data.table::setkeyv(tab, cols_tab)

           list(
             group_name = paste(group_vals, collapse=", "),
             tab = tab[order(get(cols_tab))]
           )
         })
}

#' @export
wtd.tab.flat <- function(DT, cols_tab, col_wt, cols_subs=NULL, percent=TRUE) {
  data.table::rbindlist(lapply(wtd.tab(DT, cols_tab, col_wt, cols_subs, percent),
                   function(l_subs) l_subs$tab))
}

#' @export
wtd.tab <- function(DT, cols_tab, col_wt="one", cols_subs=NULL, percent=TRUE) {
  mm <- ifelse(percent, 100, 1)

  tot <- DT[, sum(get(col_wt))]

  # All the necessary computation is done here...
  res <- DT[, .(`Number of occurrences` = sum(get(col_wt))), c(cols_tab, cols_subs)]
  res[, `Proportion of total` := mm*`Number of occurrences`/ tot]
  res[, `Proportion in group` := mm*`Number of occurrences`/ sum(`Number of occurrences`), cols_subs]
  # ... the rest is just taking this results and outputting them

  subs_gr <- unique(res[, cols_subs, cols_subs, with=FALSE]) # Find unique subsetting groups

  setkeyv(res, cols_subs)
  setkeyv(subs_gr, cols_subs)

  ## Loop over subsetting groups
  if (nrow(subs_gr) == 0) {
    return(list(list(
      group_name = "Full sample",
      tab = res[order(get(cols_tab))]
    )))
  }

  subs_names <- names(subs_gr) # Keep subsetting group names
  lapply(1:nrow(subs_gr),
         function(i) {
           ## For subset group `i`, lookup values in `res`
           this_gr <- subs_gr[i]
           tab <- this_gr[res, nomatch=0]
           group_vals <- paste(subs_names, unlist(this_gr), sep=" = ")

           setkeyv(tab, cols_tab)

           list(
             group_name = paste(group_vals, collapse=", "),
             tab = tab[order(get(cols_tab))]
           )
         })
}


colnames_verify <- function(list.DT){
  ## Inputs:
  ##   list.DT       :: (list)       :: list with data tables
  ## Outputs:
  ##   name.analysis :: (data.table) :: data table with output of column name analysis


  list.names  <- sapply(list.DT, colnames)
  maximal.name  <- Reduce(union, list.names)


  name.analysis  <- data.table(
    colname = maximal.name
  )

  in.maximal  <- lapply(list.DT, function(DT) colnames(DT) %in% maximal.name)
  setDT(in.maximal)
  name.analysis  <- cbind(name.analysis, in.maximal)
}



setGroup <- function(DT, col_group, group_list, groupvarname = NULL, ...) {
  ## Format of `group_list`:
  ## list(
  ##   group1 = c("g1_level1", "g1_level2", ...)
  ##   group2 = c("g2_level1", "g2_level2", ...)
  ## )

  ## Ellipsis are passed to the factor command


  ## Create dictionary:
  dict <- rbindlist(lapply(names(group_list), function(gname) {
    data.table(groupname = gname,
               vals = group_list[[gname]])
  }))
  # dict[, levels := factor(levels, levels=levels, ...)]
  dict[, groupname := factor(groupname, levels=unique(groupname))]



  if (is.null(groupvarname)) {
    groupvarname <- paste(col_group, "group", sep="_")
  }

  # setnames(dict, c("groupname", "vals"), c(groupvarname, col_group))
  setnames(dict, c("vals"), c(col_group))

  DT[dict, (groupvarname) := groupname, on=col_group]
}
