library(magrittr)

janno_columns <- readr::read_tsv("data-raw/janno_columns.csv")

# column names
janno_column_names <- janno_columns$janno_column_name

# column type
janno_column_name_column_type <- hash::hash(janno_columns$janno_column_name, janno_columns$column_type)

# column choices
with_choices <- janno_columns %>% dplyr::filter(
  !is.na(choice_options)
)
janno_column_name_choices <- hash::hash(with_choices$janno_column_name, with_choices$choice_options)

usethis::use_data(
  janno_column_names,
  janno_column_name_column_type,
  janno_column_name_choices,
  internal = T, overwrite = T
)
