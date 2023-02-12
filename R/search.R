#' Search for alias within PANGO lineages
#'
#' @param pangoro Pangoro S3 class object.
#' @param input Lineage name (scalar).
#' @param search Alias name to search if related to input list.
#' @param direction Search parents (\code{up}) and/or children (\code{down}) of alias (default: \code{both}).
#'
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' search_pangoro(my_pangoro, 'BA.5', 'BA.5.1')
#' mapply(c('BA.5', 'BA.5'),c('BA.5.1', 'BL.1'), FUN = function(x,y) {search_pangoro(my_pangoro,x,y)})
#' }
#'
#' @export
search_pangoro <- function(pangoro, input, search, direction = 'both') {

  # Enforce case
  direction <- tolower(direction)

  # Expand search
  exp_search <- strsplit(expand_pangoro(pangoro, search), '.', fixed = TRUE)
  exp_search_len <- sapply(exp_search, length)

  # Expand input
  exp_input <- strsplit(expand_pangoro(pangoro, input), '.', fixed = TRUE)
  exp_input_len <- sapply(exp_input, length)

  # Search parents
  if(direction == 'both' || direction == 'up'){
    if(exp_search_len < exp_input_len) {
      exp_input_trunc <- lapply(exp_input, `[`, 1:exp_search_len)
      parent_search <- purrr::map2_lgl(exp_search, exp_input_trunc, ~all(Reduce(`==`, list(.x,.y))))
    } else {parent_search <- NULL}
  } else {parent_search <- NULL}

  # Search children
  if(direction == 'both' || direction == 'down'){
    if(exp_search_len >= exp_input_len) {
      exp_search_trunc <- lapply(exp_search, `[`, 1:exp_input_len)
      child_search <- purrr::map2_lgl(exp_input, exp_search_trunc, ~all(Reduce(`==`, list(.x,.y))))
    } else {child_search <- NULL}
  } else {child_search <- NULL}

  # Combine/return
  output <- unique(c(child_search, parent_search))
  if(is.null(output)) output <- FALSE
  output

}
