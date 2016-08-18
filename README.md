[![TravisCI Build Status](https://api.travis-ci.org/SMAC-Group/TTS.svg)](https://travis-ci.org/SMAC-Group/TTS)

# A Tour of Time Series Analysis with R (`TTS`) 

Housed within this repository is the A Tour of Time Series Analysis with R (TTS) book
written using [`bookdown`](https://github.com/rstudio/bookdown). The objective
of the book is to provide an introduction to concepts related to dependent
data for 
[STAT 429: Time Series Analysis](http://catalog.illinois.edu/courses-of-instruction/stat/) 
at the [University of Illinois at Urbana-Champaign](http://www.stat.illinois.edu/)

# Helpful References

Below is a list of helpful references that were used in the construction of the 
book.

- [TeX Commands available in MathJax](http://www.onemathematicalcat.org/MathJaxDocumentation/TeXSyntax.htm) and
[how to implement macros in MathJax](http://docs.mathjax.org/en/latest/tex.html#tex-macros)
- [Bookdown Reference Guide](https://bookdown.org/yihui/bookdown)
- [Action verbs via the Revised Bloom's Taxonomy](https://www.mnstate.edu/assess/poa/actionverbs.aspx) for chapter objectives.

# License

![This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).](images/license/cc.png)

# Editing the Text

The instructions below are meant for contributors or users that wish to be able
to build the book on their own computer.

The software that is required is as follows:

1. Chiefly, make sure to download 
[GitHub for Desktop](https://desktop.github.com/).
2. Furthermore, one will need to acquire the
[RStudio Preview Build](https://www.rstudio.com/products/rstudio/download/preview/).
   - This build is different than the traditional *RStudio* client as it has cutting
   edge features. As a result, there is a high probability that something might
   go astray while using it. 
3. Install the following *R* packages:

```r
install.packages(c("devtools", "servr", "rmarkdown", "knitr"))

devtools::install_github("rstudio/bookdown")
```

Now, the first step in this process is to use the
`"Clone or desktop"` feature found on the
[book repository](https://github.com/SMAC-Group/TTS) to download its contents (
direct link: [(macOS)](github-mac://openRepo/https://github.com/SMAC-Group/TTS) |
[(windows)](github-windows://openRepo/https://github.com/SMAC-Group/TTS) ).

Once the content has been downloaded, the next step is to open the project up
in *RStudio*. To do that, open the `tts.Rproj` file located in the repository
directory that was just downloaded. Note: If you are **not** running the preview
version, then you will receive an error. 

After *RStudio* has been launched with the `tts.Rproj` file, then editing
the book is just like editing a *RMarkdown* document with a few additional
caveats. Namely, there are limitations to LaTeX characters and specific ways
for refering to figures and/or equations. Overall, each Chapter is given by
its own `.Rmd` and a starting header tag of `#`, e.g. `01-intro.Rmd`
is the first chapter with title `# Introduction`. An exception to this
is the preface being given by `index.Rmd`.

To view the book within *RStudio*, use the "Addins" drop down menu located on
the top window to launch "Preview Book". The preview option will make a live
version of the book available to be explored via the "Viewer" panel in the lower
right handside. Each time you save an `.Rmd`, the preview of the book will
update within this viewer. Make sure to *stop* the preview by clicking the
**Stop sign** icon if you are editing a code file. For some odd reason, this
seems to cause a nasty crash.


With that being said, here are a few other organizational details:

All code is located in the `/code/chapter/xx-name.R` and is read into the top
of the document using:

```{r introduction_code, echo = FALSE, cache = FALSE}
knitr::read_chunk('code/chapter/01_introduction.R')
```

All external figures (not rendered by *R*) are added to the document using:

```{r img_name, echo = FALSE, cache = TRUE}
knitr::include_graphics("images/img_name.png")
```

If you need help writing a LaTeX equation, then another Addin "Input LaTeX Math"
exists to render the equation as you type it. 

