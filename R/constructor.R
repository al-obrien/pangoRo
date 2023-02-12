#' Constructor for pangoRo object
#'
#' To minimize the number of times PANGO links are called, the first time the
#' table will be cached which can be recycled for the current session for that day..
#'
#' @inheritParams pangoro
#'
#' @noRd
new_pangoro <- function(path = character(),
                        refresh = FALSE,
                        offline = FALSE,
                        rm_spec_char = TRUE) {

  # Define cache
  time_stamp <- Sys.time()
  pangoro_cache <- file.path(tempdir(), paste0('pangoro_cache_', digest::digest(as.Date(time_stamp)), '.rds'))

  # If offline, pull from PKG, but still use cache if wanted
  if(offline) {
    message("Loading alias table from {pangoRo}'s offline copy...")
    pango_alias_tbl <- pangoro_cov_alias
    if(rm_spec_char) pango_alias_tbl$lineage <- gsub(x = pango_alias_tbl$lineage, pattern = '*', '', fixed = TRUE)
    saveRDS(pango_alias_tbl, pangoro_cache)
  } else if(!offline && refresh) { # If refresh, fetch again and save (ignore if offline..)

    message('Loading alias table from PANGO webiste...')
    pango_alias_tbl <- fetch_pango_alias(url = path)
    if(rm_spec_char) pango_alias_tbl$lineage <- gsub(x = pango_alias_tbl$lineage, pattern = '*', '', fixed = TRUE)
    saveRDS(pango_alias_tbl, pangoro_cache)

    } else if(file.exists(pangoro_cache)){ # Check cache, if present attempt read, otherwise pull fresh

      message('Loading alias table from cache...')
      pango_alias_tbl <- readRDS(pangoro_cache)

      } else {
        message('Loading alias table from PANGO webiste...')
        pango_alias_tbl <- fetch_pango_alias(url = path)
        if(rm_spec_char) pango_alias_tbl$lineage <- gsub(x = pango_alias_tbl$lineage, pattern = '*', '', fixed = TRUE)
        saveRDS(pango_alias_tbl, pangoro_cache)
      }

  structure(data.frame(pango_alias_tbl),
            fetch_timestamp = time_stamp,
            class = 'pangoro')

}

#' Create pangoro object
#'
#' Create S3 class object that will contain the necessary PANGO alias details for
#' working with other functions (expanding and collapsing names). Will cache the PANGO lineage table for
#' the current R session.
#'
#' @param path Path to PANGO lineage JSON file.
#' @param refresh Boolean value to force cache to refresh (default: \code{FALSE})
#' @param offline Boolean value, if \code{TRUE} use a saved version of alias table within package.
#' @param rm_spec_char Boolean (default: \code{TRUE})
#' @examples
#' \dontrun{
#' my_pangoro <- pangoro() # First time will fetch from web
#' my_pangoro <- pangoro() # Second time will fetch from cache
#' my_pangoro <- pangoro(refresh = TRUE) # Force cache to refresh
#' }
#' @export
pangoro <- function(path = 'https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json',
                    refresh = FALSE,
                    offline = FALSE,
                    rm_spec_char = TRUE) {

  new_pangoro(path = path, refresh = refresh, offline = offline)

}
