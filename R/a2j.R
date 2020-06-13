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
  
  anno_clean_colnames <- anno %>% janitor::clean_names()
  anno_keep <- anno_clean_colnames[, colnames(anno_clean_colnames) %in% anno2janno::colname_reference$anno]
  colnames(anno_keep) <- sapply(colnames(anno_keep), function(x) { 
    anno2janno::colname_reference$janno[anno2janno::colname_reference$anno == x] 
  }) 
  
  anno2janno::colname_reference$janno[]
   
  
  janno <- anno
  writeLines(janno, con = out_janno_path)
}
