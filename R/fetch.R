#' Fetch alias table from PANGO
#'
#' Pulls JSON and formats into a flat table. Recombinants determined by the number
#' of lineages associated with alias. Their subsequent children aliases are not flagged.
#'
#' @param url Character value defining the raw data location for the alias table.
#'
#' @examples
#' \dontrun{
#' fetch_pango_alias()
#' }
#'
#' @export
fetch_pango_alias <- function(url = 'https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json') {

  cov_alias <- jsonlite::read_json(url)

  # Alias table
  cov_alias_unnested <- pluck_unlist(cov_alias)
  num_elements <- purrr::map(cov_alias_unnested, length)

  # Combine to data.frame
  data.frame(alias = rep(names(cov_alias_unnested), num_elements),
             lineage = unlist(cov_alias_unnested, use.names = FALSE),
             recomb = rep(num_elements > 1, num_elements))

}

#' Fetch summary table from PANGO.
#'
#' Pulls what is available on the COVID-19 lineage website: \url{https://cov-lineages.org/lineage_list.html}.
#' As of the recent changes in late 2022, this website only has a annual subset of lineages. Since
#' the table is embedded within the web-page the \code{rvest} package is used to read the HTML content.
#' To provide the full lineage name, the *Description* column is searched for Alias information based upon
#' the regular expression passed to the \code{pattern} parameter. If this parameter is set to \code{NULL},
#' the alias search will be ignored and the \code{full_name} column will be excluded.
#'
#' @param url Character value defining the location of the summary table from PANGOLIN.
#' @param pattern Regular expression to search description column for alias details.
#' @param description_col Name of the description column (default: 'Description').
#' @param slice Integer value (default: 1), controls which table from the page to extract.
#' @param ... Additional parameters to send to \code{\link[rvest]{read_html}}.
#'
#' @examples
#' \dontrun{
#' fetch_pango_summary_tbl()
#' }
#' @export
fetch_pango_summary_tbl <- function(url = 'https://cov-lineages.org/lineage_list.html',
                                    pattern = '^([Aa]lias of)\\s?([A-Z\\.\\d]*)[,]?\\s?.*$',
                                    description_col = 'Description',
                                    slice = 1,
                                    ...) {

  # Fetch table (should be first table in page)
  fetch_html <- rvest::read_html(url, ...)
  covlin_tbl <- rvest::html_table(fetch_html)
  covlin_tbl <- covlin_tbl[[slice]]

  if(!is.null(pattern)) {
    # Index those matching the alias pattern
    pattern_index <- grep(covlin_tbl[[description_col]], pattern = pattern, perl = TRUE)

    # Pre allocate
    covlin_tbl$full_name <- NA_character_

    # Sub in those that matched the specific pattern
    covlin_tbl$full_name[pattern_index] <- gsub(covlin_tbl[[description_col]],
                                                pattern = pattern,
                                                replacement = '\\2',
                                                perl = TRUE)[pattern_index]
  }

  covlin_tbl

}

#' Fetch PANGO lineage notes
#'
#' Pulls the lineage notes data from the PANGO designation GitHub: \url{https://raw.githubusercontent.com/cov-lineages/pango-designation}.
#' To provide the full lineage name, the *Description* column is searched for alias information based upon
#' the regular expression passed to the \code{pattern} parameter. If this parameter is set to \code{NULL},
#' the alias search will be ignored and the \code{full_name} column will be excluded.
#'
#' @param url Character value defining the location of the lineage notes from PANGOLIN.
#' @param pattern Regular expression to search description column for alias details.
#' @param description_col Name of the description column (default: 'Description').
#'
#' @examples
#' \dontrun{
#' fetch_pango_notes()
#' fetch_pango_notes(pattern = NULL)
#' }
#' @export
# Fetch covid lineage details from more freq updated source
fetch_pango_notes <- function(url = 'https://raw.githubusercontent.com/cov-lineages/pango-designation/master/lineage_notes.txt',
                              pattern = '^([Aa]lias of)\\s?([A-Z\\.\\d]*)[,]?\\s?.*$',
                              description_col = 'Description') {

  # Load tables
  cov_desc <- utils::read.delim(url, sep = '\t')

  if(!is.null(pattern)) {
    # Index those matching the alias pattern
    pattern_index <- grep(cov_desc[[description_col]], pattern = pattern, perl = TRUE)

    # Pre allocate
    cov_desc$full_name <- NA_character_

    # Sub in those that matched the specific pattern
    cov_desc$full_name[pattern_index] <- gsub(cov_desc[[description_col]],
                                              pattern = pattern,
                                              replacement = '\\2',
                                              perl = TRUE)[pattern_index]

    # Order output cols
    col_names <- colnames(cov_desc)
    indx <- !(col_names %in% c('Lineage', 'full_name'))
    cov_desc <- cov_desc[, c('Lineage', 'full_name', col_names[indx])]
  }

  # Return final table
  cov_desc
}


#' Fetch full summary table from PANGO.
#'
#' Pulls the lineage notes data from the PANGO designation GitHub: \url{https://github.com/cov-lineages/lineages-website/blob/master/_data/lineage_data.full.json?raw=true}.
#' Compared to \code{\link{fetch_pango_summary_tbl}}, this function pulls the entire set of summary information available from PANGO JSON file.
#' To provide the full lineage name, the *Description* column is searched for alias information based upon
#' the regular expression passed to the \code{pattern} parameter. If this parameter is set to \code{NULL},
#' the alias search will be ignored and the \code{full_name} column will be excluded.
#'
#' @param url Character value defining the location of the JSON file containing the complete summary details from PANGOLIN.
#' @param pattern Regular expression to search description column for alias details.
#' @param description_col Name of the description column (default: 'Description').
#'
#' @examples
#' \dontrun{
#' fetch_pango_full_tbl()
#' fetch_pango_full_tbl(pattern = NULL)
#' }
#' @export
fetch_pango_full_tbl <- function(url = 'https://github.com/cov-lineages/lineages-website/blob/master/_data/lineage_data.full.json?raw=true',
                                 pattern = '^([Aa]lias of)\\s?([A-Z\\.\\d]*)[,]?\\s?.*$',
                                 description_col = 'Description') {
  # Import
  json_file <- jsonlite::read_json(url)

  # Create table similar to PANGO website from JSON
  covlin_tbl <- data.frame(Lineage = names(json_file),
                           'Most common countries' = pluck_col_unlist(json_file, 'Countries'),
                           'Earliest date' = pluck_col_unlist(json_file, 'Earliest date'),
                           'Latest date' = pluck_col_unlist(json_file, 'Latest date'),
                           'Number designated' = pluck_col_unlist(json_file, 'Number designated'),
                           'Number assigned'= pluck_col_unlist(json_file, 'Number assigned'),
                           Description = pluck_col_unlist(json_file, description_col),
                           check.names = FALSE,
                           stringsAsFactors = FALSE)

  if(!is.null(pattern)) {
    # Index those matching the alias pattern
    pattern_index <- grep(covlin_tbl[[description_col]], pattern = pattern, perl = TRUE)

    # Pre allocate
    covlin_tbl$full_name <- NA_character_

    # Sub in those that matched the specific pattern
    covlin_tbl$full_name[pattern_index] <- gsub(covlin_tbl[[description_col]],
                                                pattern = pattern,
                                                replacement = '\\2',
                                                perl = TRUE)[pattern_index]
  }

  # Return final table
  covlin_tbl
}
