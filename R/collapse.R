#' Collapse PANGO lineage names
#'
#' Collapse provided PANGO lineage names completely or up to a maximum depth.
#' Recombinant variants will not be collapsed. NA_character values will be carried through.
#'
#' @param pangoro Pangoro object.
#' @param input Vector of lineage names to collapse (character vector).
#' @param max_level How far to expand the lineage name (default: maximum).
#' @param simplify Boolean, pass to \code{\link{mapply}}.
#'
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' x <- c('B.1.1.529.1', 'B.1.1.529.2.75.1.2', 'BA.1', NA_character_)
#' collapse_pangoro(my_pangoro, x)
#' collapse_pangoro(my_pangoro, x, 1)
#' collapse_pangoro(my_pangoro, x, 2)
#' }
#' @export
collapse_pangoro <- function(pangoro, input, max_level = NULL, simplify = TRUE) {

  # Split
  lineage_list <- strsplit(input, '.', fixed = TRUE)
  max_steps <- sapply(lineage_list, function(x) length(x)) - 1
  num_steps <- floor( (max_steps - 1) / 3)

  # If null return maximum
  if(is.null(max_level)) {

    indx <- num_steps > 0 # Ignore if nothing to collapse
    end_max <- pmin((3 * num_steps[indx]) + 1, max_steps[indx]) # Find end point...

    alias_name <- purrr::map2(lineage_list[indx], end_max,
                              ~paste0(`[`(.x, 1:.y), collapse = '.'))
    tail_end <- purrr::map2(lineage_list[indx], end_max + 1,
                            ~paste0(`[`(.x, .y:length(.x)), collapse = '.'))

    # as.character as mapply returned list if empty (purrr?)
    input[indx] <- as.character(mapply(alias_name, tail_end,
                                       FUN = function(x, y) {
                                         tmp <- pangoro$alias[pangoro$lineage == x & pangoro$recomb == FALSE]
                                         if(length(tmp) == 0) tmp <- x # If not found in lookup (length 0) return input (i.e. top of PANGO list)
                                         paste(tmp, y, sep = '.')
                                       }, SIMPLIFY = simplify))

    # If set levels then compare num_steps to max_level
  } else {
    step_diff <- pmin(num_steps, max_level) # Take the minimum of these as max steps to use (should be 0+)
    indx <- step_diff > 0
    end_max <- pmin((3 * step_diff[indx]) + 1, max_steps[indx])

    alias_name <- purrr::map2(lineage_list[indx], end_max,
                              ~paste0(`[`(.x, 1:.y), collapse = '.'))
    tail_end <- purrr::map2(lineage_list[indx], end_max + 1,
                            ~paste0(`[`(.x, .y:length(.x)), collapse = '.'))

    input[indx] <- as.character(mapply(alias_name, tail_end,
                                       FUN = function(x, y) {
                                         tmp <- pangoro$alias[pangoro$lineage == x & pangoro$recomb == FALSE]
                                         if(length(tmp) == 0) tmp <- x # If not found in lookup (length 0) return input (i.e. top of PANGO list)
                                         paste(tmp, y, sep = '.')
                                       }, SIMPLIFY = simplify))



  }

  return(input)
}

