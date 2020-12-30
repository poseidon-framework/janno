[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub R package version](https://img.shields.io/github/r-package/v/poseidon-framework/poseidonR)
[![Travis-CI Build Status](https://travis-ci.com/poseidon-framework/poseidonR.svg?branch=master)](https://travis-ci.com/poseidon-framework/poseidonR)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidonR/master.svg)](https://codecov.io/github/poseidon-framework/poseidonR?branch=master)

# poseidonR

poseidonR is an R package to simplify the interaction with Poseidon packages. So far it mostly focusses on `.janno` files and provides a dedicated R S3 class `janno` that inherits from `tibble` and allows to tidily read and manipulate the context information stored in them.

## Installation

Install the poseidonR package from github with the following command in R:

```
if(!require('remotes')) install.packages('remotes')
remotes::install_github('poseidon-framework/poseidonR')
```

## Quickstart

### Read janno files

You can read `.janno` files with

```
my_janno_object <- poseidonR::read_janno(
  path = "path/to/my/janno_file.janno",
  to_janno = TRUE,
  validate = TRUE
)
```

The path argument takes one or multiple file paths or directory paths. `read_janno()` searches recursively for `.janno` files in the directory paths.

Before loading the `.janno` files are validated with `poseidonR::validate_janno()`. You can avoid this potentially time consuming step with `validate = FALSE`.

Usually the `.janno` files are loaded as normal `.tsv` files with every column type set to `character` and then the columns are transformed to the intended types. This transformation can be turned off with `to_janno = FALSE`.

`read_janno()` returns an object of class `janno`.

### Validate janno files

```
my_janno_issues <- validate_janno("path/to/my/janno_file.janno")
```

Returns a list of issues with the respective .janno file.

### Process age information in janno objects

```
process_age(
  my_janno_object,
  choices = c("Date_BC_AD_Prob", "Date_BC_AD_Median_Derived", "Date_BC_AD_Sample"),
  n = 100
)
```

Process age information stored in .janno files. E.g. apply sum calibration on radiocarbon dates to get proper median ages and to draw samples from the post-calibration probability distribution.

### Helper functions

```
quickcalibrate()
```

Helper function for janno file preparation

## For developers

- When pushing to this repository this [pre-commit hook shared by Robert M Flight](https://rmflight.github.io/post/package-version-increment-pre-and-post-commit-hooks) should be used to increment the R package version number in the DESCRIPTION on every commit automatically. Use `doIncrement=FALSE git commit -m "commit message"` to avoid this behaviour for individual commits.
- `tests/testthat/poseidonTestData` is a [git submodule](https://github.blog/2016-02-01-working-with-submodules/). It mirrors a certain state of [poseidon-framework/poseidonTestData](https://github.com/poseidon-framework/poseidonTestData) but has to be updated manually with `git pull`.


