# run.R - rebuilds everything with one command
#   In RStudio:  source("run.R")
#   In a shell:  Rscript run.R
# Before the first run: set source_file at the top of R/prepare_data.R

# 0. remove leftovers from an earlier, interrupted build
unlink(c(list.files("site", pattern = "\\.rmarkdown$", full.names = TRUE),
         "site/_site", "site/.quarto"), recursive = TRUE)

# 1. source file -> processed data and checks
source("R/prepare_data.R")

# 2. processed data -> six reports (the website in site/_site)
status <- system2("quarto", c("render", "site"))
if (status != 0) stop("Quarto could not build the reports - see the messages above.")

# 3. safety check of the website before it is published
source("R/check_public_site.R")
