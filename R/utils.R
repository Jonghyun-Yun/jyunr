## print strings to file
str2file <- function(fstr, fname) {
  if (file.exists(fname)) {
    ## Delete file if it exists
    file.remove(fname)
  }
  fcon <- file(fname)
  writeLines(fstr, fcon) # R to C index conversion
  close(fcon)
}

kable2file <- function(ftab, fname) {
  str2file(knitr::kable(ftab), fname)
}

list2file <- function(flist, fname) {
  if (!is.list(flist)) stop("fstr must be a list object ")
  if (file.exists(fname)) {
    ## Delete file if it exists
    file.remove(fname)
  }
  fcon <- file(fname)
  for (kk in seq_along(flist)) writeLines(flist[[kk]], fcon)
  close(fcon)
}

## stolen from https://github.com/dewittpe/qwraps2/
frmt <- function(x, digits = 2) {
  sapply(x,
         function(xx) {
           if (is.integer(xx)) {
             formatC(xx, format = "d", big.mark = ",")
           } else {
             formatC(xx, digits = digits, format = "f", big.mark = ",")
           }
         })}

## print mean and sd in latex/markdown table format
mat2mean_sd <- function(mm, sd,
                    digits = 2,
                    na_rm = FALSE,
                    show_n = "ifNA",
                    denote_sd = "pm",
                    markup = "latex") {
  n <- sum(!is.na(s))
  m = unlist(mm)
  s = unlist(sd)

  if (all(!(show_n %in% c("ifNA", "always", "never")))) {
    warning("'show_n' should be in c('ifNA', 'always', 'never').  Setting to 'ifNA'.")
    show_n <- "ifNA"
  }

  if (show_n == "always" | (show_n == "ifNA" & any(is.na(s)))) {
    rtn <- paste0(frmt(as.integer(n), digits), "; ", frmt(m, digits), " $\\pm$ ", frmt(s, digits))
  } else {
    rtn <- paste0(frmt(m, digits), " $\\pm$ ", frmt(s, digits))
  }

  if (denote_sd == "paren") {
    rtn <- gsub("\\$\\\\pm\\$\\s(.*)", "\\(\\1\\)", rtn)
  }

  if (markup == "markdown") {
    rtn <- gsub("\\$\\\\pm\\$", "&plusmn;", rtn)
  }

  rtn = as.data.frame(array(rtn, dim = dim(mm)))
  names(rtn) = names(mm)
  return(rtn)
}

## set figure size (inches)
set_fig_size = function(width, fraction=1, subplots=c(1, 1)) {
    ## Set figure dimensions to avoid scaling in LaTeX.

    ## Parameters
    ## ----------
    ## width: float or string
    ##         Document width in points, or string of predined document type
    ## fraction: float, optional
    ##         Fraction of the width which you wish the figure to occupy
    ## subplots: array-like, optional
    ##         The number of rows and columns of subplots.
    ## Returns
    ## -------
    ## fig_dim: tuple
    ##         Dimensions of figure in inches

    if (width == 'thesis') width_pt = 426.79135
    else if (width == 'beamer') width_pt = 307.28987
    else if (width == 'psychometrikaV2') width_pt = 469.75502
    else width_pt = width

    # Width of figure (in pts)
    fig_width_pt = width_pt * fraction
    # Convert from pt to inches
    inches_per_pt = 1 / 72.27

    # Golden ratio to set aesthetic figure height
    # https://disq.us/p/2940ij3
    golden_ratio = (5^.5 - 1) / 2

    # Figure width in inches
    fig_width_in = fig_width_pt * inches_per_pt
    # Figure height in inches
    fig_height_in = fig_width_in * golden_ratio * (subplots[1] / subplots[2])

    return(c(fig_width_in, fig_height_in))
}

## print cat header
cathead <- function(fstr) {
 cat("
################################################################################
### ", fstr, "
################################################################################\n")
}
