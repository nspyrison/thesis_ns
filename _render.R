library(tictoc)
library(beepr)
library(cliapp)
options(warn = 1)
tictoc::tic("Thesis compilation")
start_app(theme = simple_theme())

cli_h1("Preprocessing")

# spelling check
cli_h2("Spelling check")
rmd_files <- list.files("Rmd", pattern = "*.Rmd", full.names = TRUE, recursive = TRUE)
wordlist  <- readLines("WORDLIST")
spell_res <- spelling::spell_check_files(rmd_files, wordlist, "en_US")
if (length(spell_res$word) > 0) {
  print(spell_res)
  stop("Can you please fix typos listed above first?", call. = FALSE)
}

# convert pdf to png for html output
cli_h2("Coverting pdf to png")
fig_pdf <- list.files("figures/", pattern = "*.pdf")
for (i in fig_pdf) {
  file_pdf <- paste0("figures/", i)
  dest_pdf <- paste0("figures/", sub("pdf$", "png", i))
  if(file.exists(dest_pdf) == FALSE)
    magick::image_write(
      magick::image_read(file_pdf, 300), dest_pdf, "png",
      density = 300
  )
}

cli_h1("Compiling")
if (Sys.getenv("RSTUDIO") != "1" && Sys.info()['sysname'] == "Darwin") {
  Sys.setenv('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc')
}
# provide default formats if necessary
formats <- commandArgs(trailingOnly = TRUE)
if (length(formats) == 0)
  formats <- c("bookdown::pdf_book", "bookdown::gitbook")
# render the book to all formats
for (fmt in formats)
  bookdown::render_book("index.Rmd", fmt, quiet = FALSE)
# gif_file <- list.files("figure", "*.gif", full.names = TRUE)
# invisible(file.copy(gif_file, "_thesis/figure/animate.gif"))

cli_alert_success("You Rock!")
beepr::beep(1)
tictoc::toc()