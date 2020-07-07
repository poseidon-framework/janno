#!/usr/bin/env Rscript

'poseidon2

Utility functions for the poseidon2 data format. All input directories have to adhere to the poseidon2 package file structure as documented here: /projects1/poseidon/poseidon2.package.manager/README.md

Usage:
  poseidon2 convert <output_format> <input_package> <output_directory> [--log_directory=DIR]
  poseidon2 merge <input_file> <output_directory> [--log_directory=DIR]

Options:
  -h --help             Show this screen.
  --version             Show version.
  --log_directory=DIR   Some directory [default: ./poseidon2_tmp_and_log/current_date_random_string]

' -> doc

arguments <- docopt::docopt(doc, version = 'Poseidon 2.0.1\n')
#print(arguments)

# implements log_directory default
if (arguments$log_directory == "./poseidon2_tmp_and_log/current_date_random_string") {
  arguments$log_directory <- file.path(
    "./poseidon2_tmp_and_log",
    paste0(format(Sys.time(),'%Y-%m-%d_%H-%M'), "_", poseidon2::random_alphanumeric_string())
  )
}

# create log directory
if (!dir.exists(arguments$log_directory)) {
  dir.create(arguments$log_directory, recursive = T)
}
  
if (arguments$convert) {
  poseidon2::convert_module(
    output_format = arguments$output_format,
    input_package = arguments$input_package,
    output_directory = arguments$output_directory,
    log_directory = arguments$log_directory
  )
} else if (arguments$merge) {
  poseidon2::merge_module(
    input_file = arguments$input_file,
    output_directory = arguments$output_directory,
    log_directory = arguments$log_directory
  )
}

