# BSD_2_clause
#
# Get missing PDFs from among the links scraped from ECOS.

library(digest)
library(dplyr)
library(ecosscraper)
library(httr)
library(pdfdown)

NCORE <- detectCores() - 1

BASED <- "/datadrive/data/ECOS"
RDA <- file.path(BASED, "rda")
TMP <- file.path(BASED, "tmp_dl")
URLS <- file.path(RDA, "ECOS_species_links_2016-12-09.rda")
REFS <- file.path(RDA, "pdfs_info2016-12-10.rda")

RECPLN <- file.path(TMP, "recovery_plan")
FIVEYR <- file.path(TMP, "five_year_review")
FEDREG <- file.path(TMP, "federal_register")
CANDID <- file.path(TMP, "candidate")
CONSAG <- file.path(TMP, "conserv_agmt")
MISC <- file.path(TMP, "misc")
dirs <- c(RECPLN, FIVEYR, FEDREG, CANDID, CONSAG, MISC)
tmp_res <- lapply(dirs,
                  function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

# Get the link subsets
load(URLS)
load(REFS)

PDF_URL <- filter(ECOS_species_links, grepl(ECOS_species_links$link,
                                            pattern = "pdf$|PDF$|gpo"))
recpln_df <- filter(PDF_URL, grepl(PDF_URL$link,
                                   pattern = "recovery_plan|/pdfs/"))
fiveyr_df <- filter(PDF_URL, grepl(PDF_URL$link,
                                   pattern = "five_year_review|docs/species"))
fedreg_df <- filter(PDF_URL, grepl(PDF_URL$link,
                                   pattern = "federal_register|gpo"))
candid_df <- filter(PDF_URL, grepl(PDF_URL$link,
                                   pattern = "/candidate/"))
gotten <- "recovery_plan|/pdfs/|five_year_review|federal_register|gpo|/candidate/"
misc_df <- filter(PDF_URL, grepl(PDF_URL$link, pattern = "/misc/|/uplisting/") |
                           !grepl(PDF_URL$link, pattern = gotten))

recpln_url <- unique(recpln_df$link)
fiveyr_url <- unique(fiveyr_df$link)
fedreg_url <- unique(fedreg_df$link)
candid_url <- unique(candid_df$link)
misc_url <- unique(misc_df$link)

# make dl paths
clean_fn <- function(f) {
  f <- gsub(f, pattern = " ", replacement = "_")
  f <- ifelse(grepl(f, pattern = "pdf$|PDF$"), f, paste0(f, ".pdf"))
  return(f)
}

recpln_fil <- file.path(RECPLN, clean_fn(basename(recpln_url)))
fiveyr_fil <- file.path(FIVEYR, clean_fn(basename(fiveyr_url)))
fedreg_fil <- file.path(FEDREG, clean_fn(basename(fedreg_url)))
candid_fil <- file.path(CANDID, clean_fn(basename(candid_url)))
misc_fil <- file.path(MISC, clean_fn(basename(misc_url)))

# Try some downloads!
misc_res <- mcmapply(download_pdf, misc_url, misc_fil,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
misc_res_df <- bind_rows(misc_res)
misc_res_df$doc_type <- "misc"

recpln_res <- mcmapply(download_pdf, recpln_url, recpln_fil,
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE,
                       mc.cores = NCORE,
                       mc.preschedule = FALSE)
recpln_res_df <- bind_rows(recpln_res)
recpln_res_df$doc_type <- "recovery_plan"

fiveyr_res <- mcmapply(download_pdf, fiveyr_url, fiveyr_fil,
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE,
                       mc.cores = NCORE,
                       mc.preschedule = FALSE)
fiveyr_res_df <- bind_rows(fiveyr_res)
fiveyr_res_df$doc_type <- "five_year_review"

candid_res <- mcmapply(download_pdf, candid_url, candid_fil,
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE,
                       mc.cores = NCORE,
                       mc.preschedule = FALSE)
candid_res_df <- bind_rows(candid_res)
candid_res_df$doc_type <- "candidate"

fedreg_res <- mcmapply(download_pdf, fedreg_url, fedreg_fil,
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE,
                       mc.cores = NCORE,
                       mc.preschedule = FALSE)
fedreg_res_df <- bind_rows(fedreg_res)
fedreg_res_df$doc_type <- "federal_register"

save(misc_res_df, recpln_res_df, fiveyr_res_df,
     candid_res_df, fedreg_res_df,
     file = file.path(TMP, paste0("doc_download_", Sys.Date(), ".rda")))

# Conservation agreements (section 10) docs
load(file.path(BASED, "rda", "conservation_agmt_data_2016-12-10.rda"))
cons_agmt_docs_data <- rbind(HCP_docs, SHA_docs, CCA_docs, CCAA_docs)
cons_agmt_docs_urls <- unique(cons_agmt_docs_data$url)
cons_agmt_docs_fils <- file.path(CONSAG,
                                 clean_fn(basename(cons_agmt_docs_urls)))

consag_res <- mcmapply(download_pdf,
                       cons_agmt_docs_urls,
                       cons_agmt_docs_fils,
                       SIMPLIFY = FALSE,
                       USE.NAMES = FALSE,
                       mc.cores = NCORE,
                       mc.preschedule = FALSE)
consag_res_df <- bind_rows(consag_res)
consag_res_df$doc_type <- "conserv_agmt"

dl_data <- rbind(misc_res_df, recpln_res_df, fiveyr_res_df,
                 candid_res_df, fedreg_res_df, consag_res_df)
sum(!dl_data$pdfCheck, na.rm = TRUE)
table(dl_data$success)
non_pdfs <- filter(dl_data, !dl_data$pdfCheck)

# MD5s
dl_data$MD5 <- mclapply(dl_data$dest,
                        doc_md5,
                        mc.cores = NCORE,
                        mc.preschedule = FALSE)

dl_data_pdfs <- filter(dl_data, pdfCheck)
new_MD5 <- setdiff(dl_data_pdfs$MD5, pdfs_info$MD5)
length(new_MD5)
old_MD5 <- setdiff(pdfs_info$MD5, dl_data_pdfs$MD5)

new_to_proc <- filter(dl_data_pdfs, !(dl_data_pdfs$MD5 %in% pdfs_info$MD5))
new_to_proc <- distinct(new_to_proc, MD5, .keep_all = TRUE)
new_pdfs <- new_to_proc

save(dl_data, file=file.path(TMP, paste0("dl_data_", Sys.Date(), ".rda")))
save(new_pdfs, file=file.path(TMP, paste0("new_pdfs_", Sys.Date(), ".rda")))

