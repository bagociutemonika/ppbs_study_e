# =====================================================================
# run.R: the single rebuild command
#   In RStudio:   source("run.R")
#   In a shell:   Rscript run.R
# Before running: set the source file location in R/prepare_data.R (src_path)
# =====================================================================
# 0. remove leftovers from an earlier, interrupted render
unlink(c(list.files("site", pattern = "\\.rmarkdown$", full.names = TRUE),
         "site/_site", "site/.quarto"), recursive = TRUE)

source("R/prepare_data.R")                    # 1. source extract -> processed data + checks
status <- system2("quarto", c("render", "site"))  # 2. render all reports into site/_site
if (status != 0) stop("Quarto render failed - see messages above.")
source("R/check_public_site.R")               # 3. scan the built site before publishing
