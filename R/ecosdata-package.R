#' ecosdata: Get data and documents from ECOS
#'
#' @section About:
#'
#' The U.S. Fish and Wildlife Service provides documents (and some limited data
#' about the documents) on ECOS, \url{http://ecos.fws.gov}. But there is no API
#' for the documents, so we have to scrape the contents. Builds on
#' \link[ecosscraper]{ecosscraper} to do the "analysis" of scraping ECOS and
#' storing the information in a central location.
#'
#' @import ecosscraper
#' @import purrr
#' @importFrom digest digest
#' @importFrom dplyr filter bind_rows left_join
#' @importFrom httr http_error
#' @importFrom magrittr %>%
#' @importFrom parallel mclapply detectCores
#' @importFrom pdfdown download_pdf
#' @importFrom rvest html_attr html_node html_nodes html_table html_text
#' @importFrom xml2 read_html
#' @docType package
#' @name ecosdata
NULL
