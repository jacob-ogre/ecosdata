#' Get the HTML version of the page at a URL
#'
#' Function description; defaults to title if left blank
#'
#' @param url The URL to fetch
#' @param pause Pause for 0-3s before fetching? [default = TRUE]
#' @export
#' @examples
#' An example to be run during package build
#' \dontrun{
#'   page <- GET_page(TECP_table$Species_Page[1])
#' }
GET_page <- function(url, pause = TRUE) {
  if(pause) Sys.sleep(runif(1, 0, 3))
  page <- try(httr::GET(url))
  #try once more...
  if(class(page) == "try-error") {
    page <- try(httr::GET(url))
  }
  if(class(page) == "try-error") {
    return(NULL)
  }
  cont <- httr::content(page, as = "text")
  return(cont)
}
