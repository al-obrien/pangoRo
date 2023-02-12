#' Expand lineage from alias
#'
#' Expand lineage names from an alias to parent name up to a maximum step.
#' Recombinant variants will not be expanded. NA_character values will be carried through.
#'
#' Although several string splits occur, this function can expand 1e6 values in <2 minutes.
#'
#' @param pangoro Pangoro object.
#' @param input Vector of lineage names to expand (character vector).
#' @param max_level How far to expand the lineage name (default: maximum).
#' @param simplify Boolean, pass to \code{\link{mapply}}
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' expand_pangoro(my_pangoro, c('BA.1', 'BA.1.2.3', 'BL.2'))
#' expand_pangoro(my_pangoro, c('BA.1', 'BA.1.2.3', 'BL.2'), 1)
#' expand_pangoro(my_pangoro, c('BA.1', 'BA.1.2.3', 'BL.2'), 2)
#' expand_pangoro(my_pangoro, c('BA.1', 'BA.1.2.3', 'BL.2'), 200) # Only goes as far as it can
#' }
#' @export
expand_pangoro <- function(pangoro, input = character(), max_level = NULL, simplify = TRUE) {

  if(!is.null(max_level) && max_level < 0) stop('Invalid `max_level` parameter.')
  if(any(stats::na.omit(input) == '')) stop('Convert any blanks to NA_character_')

  # Split and get name
  provided_name <- stats::setNames(strsplit(input, '.', fixed = TRUE), input)
  alias_name <- sapply(provided_name, `[[`, 1)

  # Select highest level and return (unique helps handle NA matches, %in% instead of == helps handle NA carry over)
  expanded_name <- sapply(alias_name, function(x) unique(pangoro$lineage[pangoro$alias == x & pangoro$recomb == FALSE]))

  if(is.null(max_level)) {
    # Paste together expanded name (purrr has more stable errors for vector lengths)
    return(mapply(crt_expand_nm, input = input, exp_nm = expanded_name, prov_nm = provided_name, SIMPLIFY = simplify))
    #full_name <- purrr::pmap_chr(list(input = input, exp_nm = expanded_name, prov_nm = provided_name), crt_expand_nm)

  } else {
    split_exp <-  strsplit(expanded_name, '.', fixed = TRUE)
    max_steps <- sapply(split_exp, function(x) length(x))
    num_steps <- floor( (max_steps - 1) / 3)
    step_diff <- pmin(num_steps, max_level) # Take the minimum of these as max steps to use (should be 0+)
    indx <- step_diff > 0
    border_indx <- max_steps[indx] - (step_diff[indx] * 3) # Start location to slice exp name

    # Roll back n increments, replace expanded name with new one and continue to final glue from original name
    expanded_name[indx] <- purrr::map2(split_exp[indx], border_indx, ~paste0(`[`(.x, 1:.y), collapse = '.'))
    tail_end <- purrr::pmap(list(split_exp[indx], border_indx, max_steps[indx]), ~paste0(`[`(..1, (..2 + 1):..3), collapse = '.'))
    expanded_name[indx] <- mapply(expanded_name[indx], tail_end,
                                  FUN = function(x, y) {
                                    tmp <- pangoro$alias[pangoro$lineage == x & pangoro$recomb == FALSE]
                                    if(length(tmp) == 0) tmp <- x # If not found in lookup (length 0) return input (i.e. top of PANGO list)
                                    paste(tmp, y, sep = '.')
                                  })
    expanded_name[!indx] <- character(1) # Those with 0 are flipped to blank
    mapply(crt_expand_nm, input = input, exp_nm = expanded_name, prov_nm = provided_name, SIMPLIFY = simplify)
  }
}


