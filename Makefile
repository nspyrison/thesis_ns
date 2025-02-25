pdfbook:
  Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
  Rscript --quiet _render.R "bookdown::gitbook"

open:
  open docs/thesis.pdf

preview:
  open docs/index.html

both:
  Rscript --quiet _render.R

# response:
#   Rscript -e 'Sys.setenv("RSTUDIO_PANDOC" = "/Applications/RStudio.app/Contents/MacOS/pandoc"); rmarkdown::render("report/response.Rmd")'