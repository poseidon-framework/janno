#' upgrade_janno
#'
#' A function to upgrade janno files from old versions of the Poseidon schema.
#'
#' @param in_paths character vector. Paths to one or multiple .janno files 
#' or directories that should be recursively searched for .janno files
#' @param out_paths character vector. Must have an identical length to 
#' \code{in_paths}, also considering the recursive search for .janno files 
#' if directories are listed in \code{in_paths}. With 
#' \code{poseidonR:::get_janno_file_paths(in_paths)} the output paths will match the
#' input paths, which will cause overwriting!
#' @param in_version character vector. Poseidon schema version number of the input files. 
#' Currently only supports "2.4"
#' @param out_version character vector. Poseidon schema version number of the output files. 
#' Currently only supports "2.5"
#' @param validate logical. Run the file validation after the transformation
#' 
#' @return No output - writes files
#' @export
upgrade_janno <- function(
  in_paths,
  out_paths,
  in_version = "2.4", 
  out_version = "2.5",
  validate = TRUE
) {
  # search for janno files
  in_paths_full <- get_janno_file_paths(in_paths)
  out_paths_full <- out_paths
  # check input
  checkmate::assert_true(length(in_paths_full) == length(out_paths_full))
  checkmate::assert_subset(in_version, choices = c("2.4"))
  checkmate::assert_subset(out_version, choices = c("2.5"))
  # run transformation
  purrr::pwalk(list(
      in_paths_full, 
      out_paths_full, 
      seq_along(in_paths_full
    )), function(
      in_path, out_path, counter
  ) {
    message(counter, ": ", paste(in_path, out_path, sep = " ~> "))
    # read janno files
    in_janno <- readr::read_tsv(in_path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    # run upgrade
    out_janno <- performUpgrade24to25(in_janno)
    # write result file
    readr::write_tsv(out_janno, file = out_path, na = "n/a")
  })
  # validate output .janno files
  if (validate) { informative_validation(out_paths_full) }
}

performUpgrade24to25 <- function(x) {
  jannoNewColNames <- x
  # simple column renaming
  colnames(jannoNewColNames) <- sapply(colnames(jannoNewColNames), lookupName)
  # enable new contamination column setup
  jannoContam <- dplyr::bind_cols(jannoNewColNames, constructNewContamCols(jannoNewColNames))
  jannoContamErrorsFilled <- fillMissingContamErrors(jannoContam)
  # remove columns with only empty values
  jannoWithoutNA <- jannoContamErrorsFilled %>% dplyr::select(
    tidyselect::vars_select_helpers$where( function(x) { !isPoseidonNA(x) } )
  )
  # reorder columns
  jannoReordered <- jannoWithoutNA[janno_column_names[janno_column_names %in% colnames(jannoWithoutNA)]]
  # return new janno
  return(jannoReordered)
}

#### helper functions ####

isPoseidonNA <- function(x) { all(is_n_a(x)) }

uniteContam <- function(x, y, toUniteFirst = x, toUniteSecond = y) {
  purrr::pmap_chr(
    list(x, y, toUniteFirst, toUniteSecond), \(a, b, tF, tS) {
      if (isPoseidonNA(a) & isPoseidonNA(b)) {
        "n/a"
      } else if (isPoseidonNA(a) & !isPoseidonNA(b)) {
        paste(tS)
      } else if (!isPoseidonNA(a) & isPoseidonNA(b)) {
        paste(tF)
      } else {
        paste(tF, tS, sep = ";")
      }
    }
  )
}

colNameLookup <- hash::hash(
  c("Individual_ID", "No_of_Libraries", "Data_Type", "Nr_autosomal_SNPs", "Publication_Status", "Coverage_1240K"),
  c("Poseidon_ID", "Nr_Libraries", "Capture_Type", "Nr_SNPs", "Publication", "Coverage_on_Target_SNPs")
)

lookupName <- function(x) {
  if (x %in% hash::keys(colNameLookup)) {
    hash::values(colNameLookup, x)
  } else {
    x
  }
}

constructNewContamCols <- function(janno) {
  # start by adding empty (old) contam columns, if they are missing (technically more simple)
  if (!"Xcontam" %in% colnames(janno)) {
    janno$Xcontam <- "n/a"
    janno$Xcontam_stderr <- "n/a"
  }
  if (!"mtContam" %in% colnames(janno)) {
    janno$mtContam <- "n/a"
    janno$mtContam_stderr <- "n/a"
  }
  # construct new contam columns from the old ones
  janno %>%
    dplyr::select(
      .data[["Poseidon_ID"]],
      .data[["Xcontam"]],
      .data[["Xcontam_stderr"]],
      .data[["mtContam"]],
      .data[["mtContam_stderr"]]
    ) %>%
    dplyr::transmute(
      Contamination = uniteContam(
        .data[["Xcontam"]], .data[["mtContam"]]
      ),
      Contamination_Err = uniteContam(
        .data[["Xcontam"]], .data[["mtContam"]], .data[["Xcontam_stderr"]], .data[["mtContam_stderr"]]
      ),
      Contamination_Meas = uniteContam(
        .data[["Xcontam"]], .data[["mtContam"]], "X-based (unknown software)", "mt-based (unknown software)"
      )
    )
  
}

fillMissingContamErrors <- function(x) {
  
  # loop through the contamination columns and replace missing errors with 0.1*estimate
  for (i in 1:nrow(x)) {
    if (is_n_a(x$Contamination[i])) { next } # hacky!
    contam <- x$Contamination[i] %>% strsplit(";") %>% unlist %>% as.numeric()
    error_start <- x$Contamination_Err[i]
    error <- error_start %>% strsplit(";") %>% unlist
    error_approx <- contam[is_n_a(error)] * 0.1
    error[is_n_a(error)] <- error_approx %>% as.character()
    x$Contamination_Err[i] <- paste(error, collapse = ";")
    if (x$Contamination_Err[i] != error_start & !all(error_approx == 0)) {
      if (!"Contamination_Note" %in% colnames(x)) {
        x$Contamination_Note <- NA_character_
      }
      x$Contamination_Note[i] <- "missing errors automatically filled (0.1*the estimate)"
    }
  }
  
  return(x)
}
