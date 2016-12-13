# BSD_2_clause
#
# Create text files from all ESAdocs files

library(dplyr)
library(parallel)
library(pdftools)
library(stringr)

NCORE = detectCores() - 4

BASED <- "/datadrive/data"
SRC_D <- file.path(BASED, "ESAdocs")
TXT_D <- file.path(BASED, "ESAdocs_text")
if(!dir.exists(TXT_D)) dir.create(TXT_D)

files <- list.files(SRC_D, full.names = TRUE, recursive = TRUE)
dests <- gsub(files, pattern = "ESAdocs", replacement = "ESAdocs_text")
dests <- gsub(dests, pattern = "pdf$|PDF$", replacement = "txt")

get_text <- function(src, dest) {
  txt <- try(pdf_text(src), silent = TRUE)
  txt <- paste(txt, collapse = "^L")
  dir <- dirname(dest)
  cre <- if(!dir.exists(dir)) dir.create(dir)
  res <- try(writeLines(txt, con = dest), silent = TRUE)
  if(class(res)[1] != "try-error") {
    return(TRUE)
  }
  return(FALSE)
}

results <- mcmapply(get_text, files, dests,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE,
                    mc.cores = NCORE,
                    mc.preschedule = FALSE)
results_v <- unlist(results)

pdf_to_txt <- data_frame(pdf_file = files,
                         txt_file = dests,
                         result = results_v)

save(pdf_to_txt,
     file = file.path(TXT_D, paste0("pdf_to_txt_", Sys.Date(), ".rda")))
