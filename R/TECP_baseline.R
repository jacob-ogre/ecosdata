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
#'
#' @export
get_TECP_baseline <- function() {
  TECP_table <- get_TECP_table()
  TECP_table_MD5 <- digest(arrange(TECP_table, Scientific_Name))
  TECP_summary <- data_frame(Species = NA,
                             Page = options()$TE_list,
                             Scrape_Date = Sys.Date(),
                             Page_Text_MD5 = TECP_table_MD5)
  return(list(TECP_table = TECP_table, TECP_summary = TECP_summary))
}

