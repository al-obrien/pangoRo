#' Sort PANGO lineages
#'
#' Return ordering for PANGO lineages based upon a provided list of names which
#' may include aliases.
#'
#' @param pangoro Pangoro class object
#' @param x Vector of pango lineages
#' @param index_only Boolean value, to return index or sorted values (default: \code{FALSE}).
#' @param na.last Sort NAs top or bottom (default: TRUE, sort to top)
#' @param ... Additional parameters to \code{\link{order}}.
#'
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro()
#' x <- c('BL.2', 'BA.1', 'BA.1.2.3', 'BA.1.223.3', 'BA.1.4.3','BL.2', 'BA.1')
#' sort_pangoro(my_pangoro, x)
#' sort_pangoro(my_pangoro, x, index_only = TRUE)
#' sort_pangoro(my_pangoro, x, na.last = TRUE)
#' sort(x) # Compare to normal sort in base R
#' }
#' @export
sort_pangoro <- function(pangoro, x, index_only = FALSE, na.last = FALSE, ...){

  # Expand unique lineages and split using the alias list
  uniq_lin <- unique(x)
  uniq_lin_full <- expand_pangoro(pangoro, uniq_lin)

  # Convert numbers to actual number for sorting
  uniq_lin_full <- stats::setNames(strsplit(uniq_lin_full, '.', fixed = TRUE), uniq_lin)

  # Slice out character, integers, and recombined for sort
  max_vec <- max(sapply(uniq_lin_full, length))
  lin_char <- sapply(uniq_lin_full, `[[`, 1)
  lin_int <- sapply(uniq_lin_full, function(x) as.integer(`[`(x,2:max_vec)), simplify = TRUE)
  combined_vec <- cbind(lin_char, as.data.frame(t(lin_int)))

  # Sort by the combined vec
  combined_vec <- combined_vec[do.call(order, c(combined_vec, na.last = F, ...)),]

  # Link back to input and return order
  combined_vec$order <- seq(1:nrow(combined_vec))

  if(index_only) {
    order(combined_vec[match(x, rownames(combined_vec)), 'order'])
  } else {
    x[order(combined_vec[match(x, rownames(combined_vec)), 'order'])]
  }
}
