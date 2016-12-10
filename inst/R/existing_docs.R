# BSD_2_clause
#
# A script to calculate the names and MD5 hashes of PDFs we have already
# collected, which will then be used to determine additional documents that
# need to be scraped (if available).

library(digest)
library(ecosdata)
library(parallel)
library(tidyverse)

BASED <- "/datadrive/data/"
NCORE <- detectCores() - 1

files <- list.files(based, recursive = TRUE, full.names = TRUE)
f_info <- lapply()
MD5s <- mclapply(files, doc_md5,
                 mc.cores = NCORE,
                 mc.preschedule = FALSE)
MD5s <- unlist(MD5s)
