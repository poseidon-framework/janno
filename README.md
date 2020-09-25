[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.com/poseidon-framework/poseidon2.svg?branch=master)](https://travis-ci.com/poseidon-framework/poseidon2)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidon2/master.svg)](https://codecov.io/github/poseidon-framework/poseidon2?branch=master)

# poseidon2

Validation, merging, subsetting and convertion of Poseidon v.2 packages both in R and on the command line.

## Command line interface

**The poseidon2 command line utility is already installed on the MPI-SHH-DAG cluster, you do not have to install the software manually.**



## R interface 

...

## Installation

Install the poseidon2 R package from github with the following command in R. That allows you to use all its functions within R.

```
if(!require('remotes')) install.packages('remotes')
remotes::install_github('poseidon-framework/poseidon2')
```

If you want to use the CLI interface you can copy it to your user directory with `poseidon2::quick_install()`, make it exectutable (`chmod +x poseidon2.R`) and run it with `./poseidon2.R --help`. 

Multiple package functions require [plink1.9](https://www.cog-genomics.org/plink2) and the [eigensoft](https://github.com/DReichLab/EIG) software package to be installed on your computer.

Another way to install and use the poseidon2 command line utility is available via [singularity](https://sylabs.io/docs/). For that you have to install singularity on your computer, clone the poseidon2 repository, navigate to the `singularity` directory and run `sudo singularity build poseidon2.sif poseidon2.def`. You can then use the tool with `singularity exec poseidon2.sif /poseidon2.R --help`. plink1.9 and convertf are already installed in the singularity container and do not have to be installed separately with this solution.

## For developers

The script can be found in the `inst/` directory. A normal development workflow would be to modify the package, build it (!), and then run the script in `inst/`. Cluster deployment currently works via singularity (see `singularity`).

When pushing to this repository this [pre-commit hook shared by Robert M Flight](https://rmflight.github.io/post/package-version-increment-pre-and-post-commit-hooks) should be used to increment the R package version number in the DESCRIPTION on every commit automatically. Use `doIncrement=FALSE git commit -m "commit message"` to avoid this behaviour for individual commits.
