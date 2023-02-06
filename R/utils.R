#' Print pangoRo object
#'
#' Print the alias and lineage table from the pangoRo object.
#'
#' @param x pangoRo object.
#' @param ... Not used.
#'
#' @export
print.pangoro <- function(x, ...) {
  print(data.frame(alias = x$alias, lineage = x$lineage), ...)
}


#' Summarize pangoRo object
#'
#' Supply basic summary details of the pangoRo object.
#'
#' @param object pangoRo object.
#' @param ... Not used.
#'
#' @export
summary.pangoro <- function(object, ...) {
  writeLines(strwrap(paste0('Number of aliases available: ', unique(length(object$alias)))))
}


#' Helper function to pluck JSON table and columns
#'
#' Internal function.
#'
#' @param x Table to parse.
#' @param colnm Character value (column name in table).
#' @noRd
pluck_col_unlist <- function(x, colnm) {
  unlist(purrr::map(x, ~purrr::pluck(., colnm)), use.names = FALSE)
}


#' Helper function to pluck JSON alias data
#'
#' Internal function.
#'
#' @param x Table to parse.
#' @param colnm Character value (column name in table).
#' @noRd
pluck_unlist <- function(x) {
  name_v <- names(x)

  plucked_list <- purrr::map(name_v, ~unlist(purrr::pluck(x, .), use.names = FALSE))

  purrr::set_names(plucked_list, name_v)
}


#' Lineage expand name constructor
#'
#' Internal function to help create expanded alias names. Will return the tail end
#' of values that follow an alias if they are present.
#'
#' @param input Vector of input names.
#' @param exp_nm Vector for expanded name values.
#' @param prov_nm Vector of the provided name.
#'
#' @seealso expand_pangoro
#'
#' @noRd
crt_expand_nm <- function(input, exp_nm, prov_nm) {

  # Check if any match found or if a match is blank
  if(is.na(exp_nm) || length(exp_nm) == 0 || nchar(exp_nm) == 0) return(input)

  if(length(prov_nm) == 1) return(input)
  if(length(prov_nm) == 2) {
    return(paste(exp_nm, prov_nm[2], sep = '.'))
  } else {
    return(paste(exp_nm, paste0(prov_nm[-1], collapse = '.'), sep = '.'))
  }
}


