# Palaeoecological data-science: a data driven curriculum

A Quarto book for a semester-long course in palaeoecological data science. It runs from
obtaining data through wrangling, analysis, and visualisation, using fossil pollen from
Devil's Lake (Wisconsin, USA) as the worked example.

Authors: Quinn Asena, Jack Williams, Simon Goring, Socorro Dominguez Vidana.

## Reading the book

The rendered book is published from the `gh-pages` branch by the workflow in
`.github/workflows/publish.yml`.

## Building it locally

You need [Quarto](https://quarto.org/docs/get-started/) and R (developed against 4.4.2).

```bash
git clone https://github.com/QuinnAsena/data-driven-curriculum-quarto.git
cd data-driven-curriculum-quarto
quarto render
```

Install the packages once, up front:

```r
source("install.R")
```

Every chapter also loads what it needs with `pacman::p_load()`, which installs anything
missing, so `install.R` is a convenience rather than a requirement. It exists so that
installation happens before a lesson rather than in the middle of one.

### Which R Quarto uses

Quarto picks R off your `PATH`, which is not necessarily the R you work in. If you have more
than one version installed, check with:

```bash
quarto check knitr
```

If it reports the wrong one, point Quarto at the right R with the `QUARTO_R` environment
variable, set to the full path of `Rscript`. On Windows, in PowerShell:

```powershell
[Environment]::SetEnvironmentVariable("QUARTO_R", "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe", "User")
```

On macOS or Linux, add `export QUARTO_R=/usr/local/bin/Rscript` (or wherever `which Rscript`
points) to your shell profile.

Restart your terminal and editor afterwards; environment variables are only picked up by new
processes. Note that a project-level `_environment` file does **not** work for this: Quarto
resolves R before reading it.

### Frozen results

`_quarto.yml` sets `execute: freeze: auto`, and `_freeze/` is committed. That is what lets the
publishing workflow render without an R toolchain and without calling the Neotoma API.

**If you change a code chunk, re-render locally and commit the updated `_freeze/` with your
source change.** A stale `_freeze/` does not fail the build; it quietly publishes stale
output.

### Cached datasets

`data/` holds the formatted datasets as `.rds` files, and they are committed on purpose. Every
chapter can be run offline, and the "Respawn code" callout in each chapter shows how the file
was produced if you want to swap in your own site or proxy. Please do not remove them; their
absence has broken the build before.

## Using this for your own teaching

Fork the repository and edit to suit. The two places to start:

- `_quarto.yml` lists the chapters. Comment out the ones you are not teaching.
- The `neotoma2` and no-analogue chapters are scoped to a region. Change the site ids and the
  modern pollen dataset to re-scope the book to your own region or proxy (note that the North
  American Modern Pollen Database used in the no-analogue chapter is North America only, so
  that chapter needs a regional equivalent).

We would like to know how the resource gets adapted, so please get in touch if you use it.

## Contributing

Suggestions and corrections are welcome as issues or pull requests. The book uses British
spelling in its own prose, sentence-case headings, and a fixed chapter structure
(Background, Code, Exercises). Please keep code chunks named and unique across the book, and
remember that each chapter renders in a fresh R session, so a chapter must never depend on an
object created in another one.

## Citing

If you use this resource for teaching or learning, please cite it. DOI to follow.

## Language support

The first version is written in English. We would like to expand the resource to other
languages.
