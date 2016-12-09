# BSD_2_clause
#
# A script to collect revisit the ECOS website to check for changes.
#
# This was originally intended as an Rmarkdown document, but there appears to be
# some sort of interference between the `parallel` package and knitr...it always
# failed to knit. Argh.

starttime <- Sys.time()

library(digest)
library(dplyr)
library(ecosdata)
library(ecosscraper)
library(knitr)
library(parallel)
library(purrr)

NCORE <- detectCores() - 1
BASED <- "~/Work/Data/ECOS"
