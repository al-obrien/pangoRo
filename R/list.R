#' List parents of provided alias
#'
#' Provide a lineage and return a vector of the parent aliases. Not vectorized.
#'
#' @param pangoro Pangoro S3 class object.
#' @param input Lineage name (scalar).
#'
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' list_parents(my_pangoro, 'BL.1')
#' lapply(c('BL.1', 'BA.1', 'BE.9.1', 'BQ.4', 'XBA.1'), list_parents, pangoro = my_pangoro)
#' }
#' @export
list_parents <- function(pangoro, input) {

  if(length(input) > 1) stop('Input can only be 1 element long.')

  # Search for recomb and split
  alias_name <-  gsub(input, pattern = '^([A-Za-z]*)(\\..*)$',replacement = '\\1', perl = TRUE)

  # Search against recomb aliases, return if match found
  recomb <- sapply(alias_name, function(x) unique(pangoro$lineage[pangoro$alias == x & pangoro$recomb == TRUE]), simplify = FALSE)
  recomb_flag <- sapply(recomb, function(x) length(x) > 0)

  # Repeat if recomb (NOT vectorized)
  if(recomb_flag) {
    recomb_alises <- gsub(recomb[[1]], pattern = '^([A-Za-z]*)(\\..*)$',replacement = '\\1', perl = TRUE)
    parent_alises <- unlist(sapply(recomb[[1]], expand_parents, pangoro = pangoro, simplify = FALSE))
    return(unique(c(recomb_alises, parent_alises)))
  }

  # Expand fully to see number of steps once for non-recomb
  expand_parents(pangoro, input)
}


#' Expand parents
#'
#' Internal function for list_parents
#'
#' @noRd
expand_parents <- function(pangoro, input){
  full_name <- expand_pangoro(pangoro, input, max_level = NULL)
  split_exp <-  strsplit(full_name, '.', fixed = TRUE)
  max_steps <- sapply(split_exp, function(x) length(x))
  num_steps <- floor( (max_steps - 1) / 3)

  out <- sapply(1:num_steps, FUN = expand_pangoro, pangoro = pangoro, input = input, simplify = FALSE)
  gsub(out, pattern = '^([A-Za-z]*)(\\..*)$',replacement = '\\1', perl = TRUE)
}


#' List children of provided alias
#'
#' Provide a lineage and return a vector of the child aliases.
#'
#' @param pangoro Pangoro S3 class object.
#' @param input Lineage name (scalar).
#' @param full_names Boolean, to return expanded or alias name of children (default: \code{FALSE}).
#'
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' list_children(my_pangoro, 'BA')
#' list_children(my_pangoro, 'BL.1')
#' lapply(c('BL.1', 'BA.1', 'BA.5.1', 'BQ.4'), list_children, pangoro = my_pangoro)
#' }
#' @noRd

#TODO Have recombinants children also included
list_children <- function(pangoro, input, full_names = FALSE) {

  # Exp input and slice alias
  input_exp <- expand_pangoro(pangoro, input)
  split_input <- strsplit(input, '.', fixed = TRUE)
  alias <- sapply(split_input, `[[`, 1)

  # Length of exp input
  alis_exp_split <- strsplit(input_exp, '.', fixed = TRUE)
  len_alis_exp <- sapply(alis_exp_split, length)

  # Expand all aliases and exclude input alias
  full_exp <- expand_pangoro(pangoro, pangoro$alias)
  full_exp <- full_exp[!names(full_exp) == alias] # Drop on searched
  full_exp_split <- strsplit(full_exp, '.', fixed = TRUE)
  len_full_exp_split <- sapply(full_exp_split, length)
  full_exp_split <- full_exp_split[len_full_exp_split >= len_alis_exp]
  full_exp_split_trunc <- lapply(full_exp_split, `[`, 1:len_alis_exp) # Slice down to the max of interest to compare

  # See if any have
  output <- purrr::map2_lgl(full_exp_split_trunc, alis_exp_split, ~all(Reduce(`==`, list(.x,.y))))

  if(full_names) {
    return(full_exp[names(full_exp) %in% names(output)[output]])
  } else {
    return(names(output)[output])
  }
}


