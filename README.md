# PhD Thesis, Monash University {-}

[html version](https://nspyrison.github.io/thesis_ns/)\
[pdf version](https://github.com/nspyrison/thesis_ns/blob/master/docs/thesis_ns.pdf)

<!--
## Milestone deliverable:

Confirmation report: https://github.com/nspyrison/thesis_monash_phd/blob/master/_book/_confirmation_report_ns.pdf \
Confirmation presentation: https://github.com/nspyrison/confirmation_talk \
Mid candidature report: https://github.com/nspyrison/mid_candidature/blob/master/_mid_candidature_document.pdf \
Mid candidature presentation: https://github.com/nspyrison/mid_candidature/tree/master/_slides
-->

## Directory {-}

* `docs/`: Output folder, compiled content ends up here as .pdf and .html (open with index.html)
* `Rmd/`: R Markdown source documents for thesis document
* `figures/`: Images made with other tools to illustrate ideas.
* `data/`: Cleaned data from other repositories
* `template/`: Monash thesis template from [robjhydman/MonashThesis](https://github.com/robjhyndman/MonashThesis).
* `renv/`: Automated information of packages versions and such for reproducing this work

## Reproducibility {-}

The environment and R packages used to construct this thesis
can be recovered using the **renv** package. Run the following
code to install the packages used in this thesis:

```r
# install.packages("renv")
renv::restore()
```

The thesis can be compiled into both html and pdf format. This can be done in RStudio > Build pane > `Build Book` or run the following codes in the terminal:

```zsh
Rscript --quiet _render.R "bookdown::pdf_book"
Rscript --quiet _render.R "bookdown::gitbook"
```

------

## Dev notes {-}

* `! LaTeX Error: Missing \begin{document}` is caused by a rouge .aux file; delete root level .aux file & rebuild. Thanks to Dan Simpson via NUMBAT slack.
* `! Package biblatex Error: Nested citation command.` is caused by reference citations.
[@ref1, @ref2] needs to change to [@ref1; @ref2] to work; find and replace, rebuild.
* `_bookdown.yml`: Higher level bookdown YAML settings (& specifies where/which rmd files)
* `_render.R`: Code ran at build; spell check, convert .pdf to .png, `bookdown::render_book`
    * _nb_: .png only created if it doesn't exist in `docs/figures`; delete/clean when updating figures.
* `./index.Rmd`: Knitr setup and local functions. MUST print a splash page content for HTML version to work
* `template/monashthesis.cls`: Styling template and title page
* `template/monashthesis.tex`: LaTeX packages and preamble


## Session info {-}

```
R> sessionInfo()
R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252   
```
