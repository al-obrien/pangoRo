#' Search for alias within PANGO lineages
#'
#' @param pangoro Pangoro S3 class object.
#' @param input Lineage name (scalar).
#' @param search Alias name to search if related to input list.
#' @param direction Search parents (\code{up}) and/or children (\code{down}) of alias (default: \code{both}).
#'
#' @export
search_pangoro <- function(pangoro, input, search, direction = 'both') {

  # Search parents
  if(direction == 'both' || direction == 'up'){

    parent_search <- list_parents(pangoro, input)

  }

  # Search children
  if(direction == 'both' || direction == 'down'){

    child_search <- list_children(pangoro, input)

  }

  # Combine/return
  search %in% unique(parent_search, child_search)

}
