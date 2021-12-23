################################################################################
### better default for my tastes
################################################################################

## * suppress readr the column guessing printing
read_csv = function(...) readr::read_csv(..., col_types = readr::cols())
read_csv2 = function(...) readr::read_csv2(..., col_types = readr::cols())
read_delim = function(...) readr::read_delim(..., col_types = readr::cols())
read_tsv = function(...) readr::read_tsv(..., col_types = readr::cols())
