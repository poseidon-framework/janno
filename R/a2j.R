#' Title
#'
#' @param in_anno_path test
#' @param out_janno_path test
#'
#' @export
a2j <- function(
  in_anno_path, 
  out_janno_path = file.path(dirname(in_anno_path), gsub(".anno", ".janno", basename(in_anno_path)))
) {
  anno <- readLines(in_anno_path)
  janno <- anno
  writeLines(janno, con = out_janno_path)
}
