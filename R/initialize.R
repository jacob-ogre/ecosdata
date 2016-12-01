#' Get a baseline hash of the ECOS species report table (TECP_table)
#'
#' Fetches the table at \url{https://goo.gl/tZRu5o}, and returns both the table
#' as a data.frame and a summary of the scrape including the MD5 of the table
#' (same structure as returned by \link[ecosscraper]{get_species_page_summary}).
#'
#' @return A list with two elements, \describe{
#'   \item{TECP_table}{The table of T, E, candidate, and proposed species}
#'   \item{TECP_summary}{A data.frame with summary information from the scrape}
#' }
#' @export
get_TECP_baseline <- function() {
  TECP_table <- get_TECP_table()
  TECP_table_MD5 <- digest(arrange(TECP_table, Scientific_Name))
  TECP_summary <- data.frame(Species = NA,
                             Page = options()$TE_list,
                             Scrape_Date = Sys.Date(),
                             Page_Text_MD5 = TECP_table_MD5,
                             stringsAsFactors = FALSE)
  return(list(TECP_table = TECP_table, TECP_summary = TECP_summary))
}

#' Get baseline hashes of species' ECOS pages
#'
#' @param urls The species page URLs to fetch
#' @param species The scientific names of the species to process
#' @param pause Whether to pause 0.5-3s between page fetches [default = FALSE]
#' @export
#' @examples
#' \dontrun{
#'    sp_base <- get_species_baseline(TECP_table$Species_Page[1:3],
#'                                    TECP_table$Scientific_Name[1:3],
#'                                    FALSE)
#' }
get_species_baseline <- function(urls, species, pause = FALSE) {
  if(length(pause) == 1) pause <- rep(pause, length(urls))
  args <- list(urls, species, pause)
  sp_base <- pmap(args, get_species_page_summary_)
  return(sp_base)
}
