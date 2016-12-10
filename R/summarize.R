#' Get a summary data.frame of an ECOS page scrape
#'
#' Note the trailing underscore, to separate from
#' \link[ecosscraper]{get_species_page_summary}
#'
#' By default, \code{get_species_page_summary} will fetch the species' ECOS page
#' given the URL, but there may be times when the page has already been fetched.
#' In those cases, the page can be specified to save time.
#'
#' @note Apparently, FWS does not serve the same version of a species' page up
#' twice in a row. Instead, the same information will be presented in different
#' orders. We have to use \code{strsplit} along with \link[stringr]{str_trim} to
#' get clean lines, then sort before doing the MD5 hash.
#'
#' @param url The url of the species page
#' @param species The scientific name of the species
#' @param pause Pause for 0.5-3s during scraping [default = TRUE]
#' @export
get_species_page_summary_ <- function(url, species, pause = TRUE) {
  pg <- get_species_page(url)
  page_txt <- html_text(pg)
  page_txt <- unlist(strsplit(page_txt, split = "\n"))
  page_txt <- unlist(stringr::str_trim(page_txt))
  page_txt <- sort(page_txt)
  md5_hash <- digest(page_txt)
  tab_1 <- data.frame(Species = species,
                      Page = url,
                      Scrape_Date = Sys.Date(),
                      Page_Text_MD5 = md5_hash,
                      stringsAsFactors = FALSE)
  return(tab_1)
}

#' Calculate the MD5 digest for an ECOS page
#'
#' @note Apparently, FWS does not serve the same version of a species' page up
#' twice in a row. Instead, the same information will be presented in different
#' orders. We have to use \code{strsplit} along with \link[stringr]{str_trim} to
#' get clean lines, then sort before doing the MD5 hash.
#'
#' @param f Path to a species' ECOS page (either local or URL)
#' @export
#' @examples
#' \dontrun{
#'   md5 <- species_page_md5("https://goo.gl/kvIcH9")
#' }
species_page_md5 <- function(f) {
  txt <- readLines(f)
  txt <- unlist(stringr::str_trim(txt))
  txt <- sort(txt)
  md5_hash <- digest(txt)
  return(md5_hash)
}

#' Calculate the MD5 digest for a file
#'
#' @details A simple wrapper over \link[digest]{digest}, with \code{file = TRUE}
#' and \code{errormode = "warn"}.
#'
#' @param f Path to a file
#' @export
#' @examples
#' \dontrun{
#'   md5 <- doc_md5("~/Downloads/test.pdf")
#' }
doc_md5 <- function(f) {
  digest(f, file = TRUE, errormode = "warn")
}

#' Slightly improved \link[base]{file.info}
#'
#' @details Move the file name from rowname and into \code{$path} of
#' \link[tidyr]{data_frame}.
#'
#' @param ... Paths to one or more files to gather info
#' @export
#' @examples
#' \dontrun{
#'   info <- file_info("~/Downloads/test.pdf")
#' }
file_info <- function(...) {
  info <- file.info(...)
  info$path <- path.expand(rownames(info))
  rownames(info) <- seq_along(info$path)
  info <- as_data_frame(info)
  info <- info[, c(length(info), 1:(length(info)-1))]
  return(info)
}
