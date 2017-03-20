# BSD_2_clause
#
# A script to collect baseline data on the ECOS website.
#
# This was originally written as an Rmarkdown document, but there appears to be
# some sort of interference between the `parallel` package and knitr...it always
# failed to knit. Argh.

starttime <- Sys.time()

library(digest)
library(dplyr)
library(ecosdata)
library(ecosscraper)
library(knitr)
library(parallel)
library(pdfdown)
library(purrr)

NCORE <- detectCores() - 1
BASED <- "/datadrive/data/ECOS_2"
RDAs <- file.path(BASED, "rda")
if(!dir.exists(BASED)) dir.create(BASED)
if(!dir.exists(RDAs)) dir.create(RDAs)

# TECP baseline: table of threatened, endangered, candidate, and proposed spp.
TECP_init <- get_TECP_baseline()
TECP_table <- TECP_init$TECP_table
TECP_summary <- TECP_init$TECP_summary
save(TECP_table,
     file = file.path(BASED, "rda", paste0("TECP_table_", Sys.Date(), ".rda")))
save(TECP_summary,
     file = file.path(BASED, "rda", paste0("TECP_summary_", Sys.Date(), ".rda")))

# Download ECOS page
urls <- TECP_table$Species_Page
dirs <- file.path(BASED, "species", TECP_table$Species_Code)
res <- lapply(dirs, function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))
fils <-  file.path(dirs, paste0(TECP_table$Species_Code,
                                "_", Sys.Date(), ".html"))
results <- mcmapply(download_species_page,
                    urls, fils,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE,
                    mc.cores = NCORE,
                    mc.preschedule = FALSE)
results <- bind_rows(results)
results$species <- TECP_table$Scientific_Name
ECOS_dl <- results

# MD5 hashes
files <- fils
md5s <- mclapply(files,
                 species_page_md5,
                 mc.cores = NCORE,
                 mc.preschedule = FALSE)
ECOS_dl$MD5 <- unlist(md5s)
save(ECOS_dl,
     file = file.path(BASED, "rda",
                      paste0("ECOS_dl_", Sys.Date(), ".rda")))

# Links on each page
sp_links <- mclapply(files,
                     get_species_links,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
ECOS_species_links <- bind_rows(sp_links)
save(ECOS_species_links,
     file = file.path(BASED, "rda",
                      paste0("ECOS_species_links_", Sys.Date(), ".rda")))

# Tables on each page
tabs <- mclapply(files,
                 get_species_tables,
                 mc.cores = NCORE,
                 mc.preschedule = FALSE)
tab_names <- map(1:length(tabs), function(x) names(tabs[[x]])) %>%
               unlist() %>% unique()
names(tabs) <- c(as.character(seq(1, length(tabs))))
species_table <- bind_tables(tabs, "SP_TAB")
fedreg_table <- bind_tables(tabs, "FR_TAB")
recovery_table <- bind_tables(tabs, "REC_TAB")
adddoc_table <- bind_tables(tabs, "DOC_TAB")
fiveyr_table <- bind_tables(tabs, "REV_TAB")
crithab_table <- bind_tables(tabs, "CH_TAB")
HCP_table <- bind_tables(tabs, "HCP Plan Summaries")
SHA_table <- bind_tables(tabs, "SHA Plan Summaries")
CCA_table <- bind_tables(tabs, "CCA Plan Summaries")
CCAA_table <- bind_tables(tabs, "CCAA Plan Summaries")
save(species_table, fedreg_table, recovery_table,
     adddoc_table, fiveyr_table, crithab_table,
     HCP_table, SHA_table, CCA_table, CCAA_table,
     file = file.path(BASED, "rda",
                      paste0("ECOS_species_tables_", Sys.Date(), ".rda")))

# Petitions tables
TSN <- unlist(lapply(files, get_species_tsn))
petitions_table <- mclapply(TSN, get_petitions_table,
                            mc.cores = NCORE,
                            mc.preschedule = FALSE)
names(petitions_table) <- ECOS_dl$species
petitions_table <- bind_rows(petitions_table)
save(petitions_table,
     file = file.path(BASED, "rda",
                      paste0("petitions_table_", Sys.Date(), ".rda")))

load(file.path(BASED, "rda", paste0("petitions_table_", Sys.Date(), ".rda")))
pet_url <- petitions_table$Petition_Doc_URL
pet_url <- unique(unlist(lapply(pet_docs, strsplit, split = "; ")))
pet_fils <- basename(pet_url)
petition_downs <- file.path(BASED, "")

# County occurrence
co_urls <- filter(ECOS_species_links,
                  grepl(ECOS_species_links$link, pattern = "countiesBySpecies"))
counties_tabs <- mcmapply(get_counties,
                          co_urls$link, co_urls$Scientific_Name,
                          SIMPLIFY = FALSE,
                          mc.cores = NCORE,
                          mc.preschedule = FALSE)
counties_table <- bind_rows(counties_tabs)
save(counties_table,
     file = file.path(BASED, "rda",
                      paste0("counties_table_", Sys.Date(), ".rda")))


# Conservation agreements data and links to files
HCP_url <- HCP_table$Doc_Link
HCP_spp <- HCP_table$Species
HCP_data <- mcmapply(get_conservation_plan_data,
                     HCP_url, HCP_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
HCP_data <- bind_rows(HCP_data)

HCP_docs <- mcmapply(get_conservation_plan_doc_links,
                     HCP_url, HCP_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
HCP_docs <- bind_rows(HCP_docs)

SHA_url <- SHA_table$Doc_Link
SHA_spp <- SHA_table$Species
SHA_data <- mcmapply(get_conservation_plan_data,
                     SHA_url, SHA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
SHA_data <- bind_rows(SHA_data)

SHA_docs <- mcmapply(get_conservation_plan_doc_links,
                     SHA_url, SHA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
SHA_docs <- bind_rows(SHA_docs)

CCA_url <- CCA_table$Doc_Link
CCA_spp <- CCA_table$Species
CCA_data <- mcmapply(get_conservation_plan_data,
                     CCA_url, CCA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
CCA_data <- bind_rows(CCA_data)

CCA_docs <- mcmapply(get_conservation_plan_doc_links,
                     CCA_url, CCA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
CCA_docs <- bind_rows(CCA_docs)

CCAA_url <- CCAA_table$Doc_Link
CCAA_spp <- CCAA_table$Species
CCAA_data <- mcmapply(get_conservation_plan_data,
                     CCAA_url, CCAA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
CCAA_data <- bind_rows(CCAA_data)

CCAA_docs <- mcmapply(get_conservation_plan_doc_links,
                     CCAA_url, CCAA_spp,
                     SIMPLIFY = FALSE,
                     USE.NAMES = FALSE,
                     mc.cores = NCORE,
                     mc.preschedule = FALSE)
CCAA_docs <- bind_rows(CCAA_docs)

save(HCP_data, HCP_docs, SHA_data, SHA_docs, CCA_data, CCA_docs,
     CCAA_data, CCAA_docs,
     file = file.path(BASED, "rda",
                      paste0("conservation_agmt_data_", Sys.Date(), ".rda")))

Sys.time() - starttime
