# PhD Thesis, Monash University
---------

[html version](https://nspyrison.github.io/thesis_ns/)\
[pdf version](https://github.com/nspyrison/thesis_ns/blob/master/docs/thesis_ns.pdf)

<!--
## Milestone deliverable:

Confirmation report: https://github.com/nspyrison/thesis_monash_phd/blob/master/_book/_confirmation_report_ns.pdf \
Confirmation presentation: https://github.com/nspyrison/confirmation_talk \
Mid canidature report: https://github.com/nspyrison/mid_candidature/blob/master/_mid_candidature_document.pdf \
Mid canidature presentation: https://github.com/nspyrison/mid_candidature/tree/master/_slides
--->

## Directory

* `docs/`: Output folder, compiled content ends up here as .pdf and .html (open with index.html).
* `Rmd/`: R Markdown source documents for thesis document.
* `figures/`: Images made with other tools to illustrate ideas. 
* `data/`: Cleaned data from other repositories.
* `template/`: Monash thesis template from [robjhydman/MonashThesis](https://github.com/robjhyndman/MonashThesis).
* `renv/`: Automated information of packages versions and such for reproducing this work.

## Reproducibility

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

### Endpoint info
```
R> xfun::session_info()
R version 4.1.1 (2021-08-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042), RStudio 1.4.1717
```
