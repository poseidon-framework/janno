[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.com/poseidon-framework/poseidon2.svg?branch=master)](https://travis-ci.com/poseidon-framework/poseidon2)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidon2/master.svg)](https://codecov.io/github/poseidon-framework/poseidon2?branch=master)

# poseidon2

A CLI tool for merging, convertion and validation of poseidon2 packages.

**poseidon2 is already installed on the MPI-SHH-DAG cluster, you do not have to install the software manually.** 

## Installation

1. Install poseidon2 R package from github within R 

```
if(!require('remotes')) install.packages('remotes')
remotes::install_github('poseidon-framework/poseidon2')
```

2. Install the script in your user directory with `poseidon2::quick_install()` within R
3. Make the main script file executable with `chmod +x poseidon2.R`
4. Run it with `./poseidon2.R --help`

## For developers

The script can be found in the `inst/` directory. A normal development workflow would be to modify the package, build it (!), and then run the script in `inst/`. Cluster deployment currently works via singularity (see `singularity/`).

When pushing to this repo this [pre-commit hook shared by Robert M Flight](https://rmflight.github.io/post/package-version-increment-pre-and-post-commit-hooks) should be used to increment the R package version number in the DESCRIPTION on every commit automatically. Use `doIncrement=FALSE git commit -m "commit message"` to avoid this behaviour on individual instances. We test this feature right now and see if it is useful.
