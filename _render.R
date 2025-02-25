# Build Book -----

## Do use print verbose .log info?
if(FALSE) options(tinytex.verbose = TRUE)
## If you are getting ! LaTeX Error: Missing \begin{document}
# it is likely a rogue .aux file. try to delete them and rebuild.
# - Thanks to Dan Simpson on the NUMBAT slack.

## Setup ----
library(tictoc)
library(beepr)
library(cliapp)
prev_warn <- getOption("warn")
options(warn = 1)
tictoc::tic("Thesis compilation")
start_app(theme = simple_theme())
cli_h1("Preprocessing")


## Spelling check -----
cli_h2("Spelling check")
rmd_files <- list.files("Rmd", pattern = "*.Rmd", full.names = TRUE, recursive = TRUE)
wordlist  <- readLines("WORDLIST")
spell_res <- spelling::spell_check_files(rmd_files, wordlist, "en_US")
if (length(spell_res$word) > 0) {
  print(spell_res)
  stop("Can you please fix typos listed above first?", call. = FALSE)
}


## Image conversion -----
#### Convert pdf to png for html output
cli_h2("Coverting pdf to png")
fig_pdf <- list.files("figures/", pattern = "*.pdf")
for (i in fig_pdf) {
  file_pdf <- paste0("figures/", i)
  dest_pdf <- paste0("figures/", sub("pdf$", "png", i))
  ## If a .png of the same name does not exist make it from .pdf.
  if(file.exists(dest_pdf) == FALSE)
    magick::image_write(
      magick::image_read(file_pdf, 300),
      dest_pdf, "png", density = 300
    )
}


## Compilation ----
#### handle OS and formats
cli_h1("Compiling")
if (Sys.getenv("RSTUDIO") != "1" && Sys.info()["sysname"] == "Darwin")
  Sys.setenv("RSTUDIO_PANDOC" = "/Applications/RStudio.app/Contents/MacOS/pandoc")
# provide default formats if necessary
formats <- commandArgs(trailingOnly = TRUE)
if (length(formats) == 0)
  formats <- c("bookdown::pdf_book", "bookdown::gitbook")
# render the book to all formats
for (fmt in formats)
  bookdown::render_book("index.Rmd", fmt, quiet = FALSE)

cli_alert_success(praise::praise())
cli_alert_info(paste0("Compiled at ", Sys.time()))
options(warn = prev_warn)
tictoc::toc()
beepr::beep()