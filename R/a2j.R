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
  anno <- readr::read_tsv("testdata/1240K.anno.mod2", na = c("..", "n/a"))
  
  
  
  janno <- anno
  writeLines(janno, con = out_janno_path)
}
