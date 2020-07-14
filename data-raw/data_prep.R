library(magrittr)

janno_columns <- readr::read_tsv(
  "https://raw.githubusercontent.com/poseidon-framework/poseidon2-schema/2.0.0/janno_columns.tsv"
)

# column names
janno_column_names <- janno_columns$janno_column_name

# column type
janno_column_name_column_type <- hash::hash(janno_columns$janno_column_name, janno_columns$column_type)

# column choices
with_choices <- janno_columns %>% dplyr::filter(
  !is.na(choice_options)
)
janno_column_name_choices <- hash::hash(with_choices$janno_column_name, with_choices$choice_options)

# choice columns
janno_choice_columns <- with_choices$janno_column_name

# mandatory columns
janno_mandatory_columns <- janno_columns$janno_column_name[janno_columns$mandatory]

# unique columns
janno_unique_columns <-  janno_columns$janno_column_name[janno_columns$unique]

# column ranges
with_ranges <- janno_columns %>% dplyr::filter(
  !is.na(range_lower) & !is.na(range_upper) 
)
janno_column_name_range_lower <- hash::hash(with_ranges$janno_column_name, with_ranges$range_lower)
janno_column_name_range_upper <- hash::hash(with_ranges$janno_column_name, with_ranges$range_upper)
janno_range_columns <- with_ranges$janno_column_name

usethis::use_data(
  janno_column_names,
  janno_column_name_column_type,
  janno_column_name_choices,
  janno_choice_columns,
  janno_mandatory_columns,
  janno_unique_columns,
  janno_column_name_range_lower,
  janno_column_name_range_upper,
  janno_range_columns,
  internal = T, overwrite = T
)
