# BSD_2_clause
#
# A script to calculate the names and MD5 hashes of PDFs we have already
# collected, which will then be used to determine additional documents that
# need to be scraped (if available).

library(digest)
library(ecosdata)
library(parallel)
# library(tidyverse)

BASED <- "/datadrive/data"
NCORE <- detectCores() - 1

pdfs <- list.files(BASED,
                   pattern = "pdf|PDF",
                   recursive = TRUE,
                   full.names = TRUE)
pdfs_info <- file_info(pdfs)
MD5s <- mclapply(pdfs,
                 doc_md5,
                 mc.cores = NCORE,
                 mc.preschedule = FALSE)
MD5s <- unlist(MD5s)
pdfs_info$MD5 <- MD5s
pdfs_info$file_name <- basename(pdfs_info$path)

save(pdfs_info,
     file = file.path(BASED, paste0("pdfs_info", Sys.Date(), ".rda")))
