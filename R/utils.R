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
  if (!is.list(flist)) error("fstr must be a list object ")
  if (file.exists(fname)) {
    ## Delete file if it exists
    file.remove(fname)
  }
  fcon <- file(fname)
  for (kk in 1:length(flist)) writeLines(flist[[kk]], fcon)
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

  rtn = as.data.frame(array(rtn,dim=dim(mm)))
  names(rtn) = names(mm)
  return(rtn)
}
