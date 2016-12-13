# BSD_2_clause
#
# Find most-likely associations between already OCR'd docs and originals.

library(dplyr)
library(parallel)
library(pdftools)
library(pdftext)

NCORE <- detectCores() - 1

BASED <- "/datadrive/data"
ESADOC <- file.path(BASED, "ESAdocs")
FEDREG <- file.path(ESADOC, "federal_register")
FIVEYR <- file.path(ESADOC, "five_year_review")
RECPLN <- file.path(ESADOC, "recovery_plan")
CONSUL <- file.path(ESADOC, "consultation")
CANDID <- file.path(ESADOC, "candidate")
CONSAG <- file.path(ESADOC, "conserv_agmt")
POLICY <- file.path(ESADOC, "policy")
MISC_D <- file.path(ESADOC, "misc")
new_dirs <- c(FEDREG, FIVEYR, RECPLN, CONSUL, CANDID, CONSAG, POLICY, MISC_D)
lapply(new_dirs, function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

# First, the new pdfs, based on work in get_missing_pdfs.R
new_pdfs <- file.path(BASED, "ECOS", "tmp_dl", "new_pdfs_2016-12-11.rda")
load(new_pdfs)
new_ECOS_pdfs <- new_pdfs$dest

# Now the originals
ORIGD <- file.path(BASED, "NLP")
all_orig <- list.files(ORIGD, recursive = TRUE, full.names = TRUE)
all_orig <- all_orig[-grep(all_orig,
                           pattern = "dupes|ERRs|IMGs|PAGEs|TXTs|.rda$|.txt$")]
test_pdf <- mclapply(all_orig, is_pdf,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
test_pdf <- unlist(test_pdf)
orig_files <- data_frame(path = all_orig,
                         file = basename(all_orig),
                         isPdf = test_pdf)
to_proc_orig <- filter(orig_files, isPdf)

# the files to OCR or confirm embed...
to_proc_all <- c(new_ECOS_pdfs, to_proc_orig$path)

make_ocr_file_path <- function(f, based) {
  cleannm <- basename(f) %>%
               gsub(pattern = " ", replacement = "_") %>%
               gsub(pattern = "&", replacement = "and") %>%
               gsub(pattern = "\\(|\\)", replacement = "") %>%
               gsub(pattern = "\\,", replacement = "")
  if(grepl(f, pattern = "federal_register")) {
    file.path(based, "federal_register", cleannm)
  } else if(grepl(f, pattern = "five_year_review")) {
    file.path(based, "five_year_review", cleannm)
  } else if(grepl(f, pattern = "[Rr]ecovery_[Pp]lan")) {
    file.path(based, "recovery_plan", cleannm)
  } else if(grepl(f, pattern = "section_7a2|consultation")) {
    file.path(based, "consultation", cleannm)
  } else if(grepl(f, pattern = "conserv_agmt")) {
    file.path(based, "conserv_agmt", cleannm)
  } else if(grepl(f, pattern = "candidate")) {
    file.path(based, "candidate", cleannm)
  } else if(grepl(f, pattern = "HCP|SHA")) {
    file.path(based, "conserv_agmt", cleannm)
  } else if(grepl(f, pattern = "policies")) {
    file.path(based, "policy", cleannm)
  } else if(grepl(f, pattern = "[Mm]isc")) {
    file.path(based, "misc", cleannm)
  } else {
    stop(paste("Missing pattern:", f))
  }
}

to_proc_dest <- unlist(lapply(to_proc_all,
                              make_ocr_file_path,
                              based = ESADOC))

for_OCR <- data_frame(infile = to_proc_all,
                      outfile = to_proc_dest)

save(for_OCR, file = file.path(BASED, paste0("for_OCR_", Sys.Date(), ".rda")))

wrap_ocrmypdf <- function(infile, outfile) {
  cmd <- paste0("ocrmypdf ",
                "--deskew ",
                "-j 2 ",
                "--rotate-pages --rotate-pages-threshold 10 ",
                "--oversample 500 ",
                "--skip-text ",
                "-l eng --tesseract-config ~/asciimostly '",
                infile,
                "' ",
                outfile)
  if(!file.exists(outfile)) {
    message(sprintf("\n\tProcessing %s; writing to %s...\n", infile, outfile))
    res <- try(pdftools::pdf_text(infile), silent = TRUE)
    if(class(res) != "try-error") {
      nchars <- unlist(lapply(res, nchar))
      if(mean(nchars, na.rm = TRUE) > 1000) {
        file.copy(infile, outfile)
        cur_res <- data_frame(infile = infile,
                              outfile = outfile,
                              error = "File copied",
                              proc_time = Sys.time())
        return(cur_res)
      }
    }
    res <- try(system(command = cmd, intern = FALSE, wait = TRUE), silent = TRUE)
    if(class(res) == "try-error") {
      error <- res
    } else {
      error <- NA
    }
    cur_res <- data_frame(infile = infile,
                          outfile = outfile,
                          error = error,
                          proc_time = Sys.time())
    return(cur_res)
  } else {
    cur_res <- data_frame(infile = infile,
                          outfile = outfile,
                          error = "File exists",
                          proc_time = Sys.time())

    return(cur_res)
  }
}

test <- wrap_ocrmypdf(for_OCR$infile[1], for_OCR$outfile[1])

new_OCR <- mcmapply(wrap_ocrmypdf, for_OCR$infile, for_OCR$outfile,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE,
                    mc.cores = 12,
                    mc.preschedule = FALSE)
OCR_res <- bind_rows(new_OCR)
OCR_res$error <- ifelse(is.na(OCR_res$error),
                        "File processed",
                        OCR_res$error)

OCR_res$OCR_MD5 <- mclapply(OCR_res$outfile,
                            doc_md5,
                            mc.cores = NCORE,
                            mc.preschedule = FALSE)
OCR_res$OCR_MD5 <- unlist(OCR_res$OCR_MD5)

get_n_pages <- function(f) {
  txt <- try(pdf_text(f), silent = TRUE)
  if(class(txt)[1] != "try-error") return(length(txt)) else return(NA)
}

OCR_res$n_pages <- mclapply(OCR_res$outfile,
                            get_n_pages,
                            mc.cores = NCORE,
                            mc.preschedule = FALSE)
OCR_res$n_pages <- unlist(OCR_res$n_pages)

save(OCR_res, file = file.path(BASED, paste0("OCR_res_", Sys.Date(), ".rda")))
