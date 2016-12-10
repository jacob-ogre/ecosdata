# BSD_2_clause
#
# A script to calculate the names and MD5 hashes of PDFs we have already
# collected, which will then be used to determine additional documents that
# need to be scraped (if available).

library(digest)
BASED <- "/datadrive/data/"

files <- list.files(based, recursive = TRUE, full.names = TRUE)
