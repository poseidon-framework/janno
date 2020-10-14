janno_columns <- readr::read_tsv(
  "https://raw.githubusercontent.com/poseidon-framework/poseidon2-schema/master/janno_columns.tsv"
)

# column names
janno_column_names <- janno_columns$janno_column_name

# data type
janno_column_name_data_type <- hash::hash(janno_columns$janno_column_name, janno_columns$data_type)

# multi value columns
janno_multi_value_columns <- janno_columns$janno_column_name[janno_columns$multi]

# choice columns
janno_choice_columns <- janno_columns$janno_column_name[janno_columns$choice]

# column choices
choices <- janno_columns$choice_options[janno_columns$choice]
janno_column_name_choices <- hash::hash(janno_choice_columns, choices)

# mandatory columns
janno_mandatory_columns <- janno_columns$janno_column_name[janno_columns$mandatory]

# unique columns
janno_unique_columns <- janno_columns$janno_column_name[janno_columns$unique]

# bonus columns
janno_bonus_columns <- janno_columns$janno_column_name[janno_columns$bonus]

# range columns
janno_range_columns <- janno_columns$janno_column_name[janno_columns$range]

# column ranges
lower_ranges <- janno_columns$range_lower[janno_columns$range]
upper_ranges <- janno_columns$range_upper[janno_columns$range]
janno_column_name_range_lower <- hash::hash(janno_range_columns, lower_ranges)
janno_column_name_range_upper <- hash::hash(janno_range_columns, upper_ranges)

usethis::use_data(
  janno_column_names,
  janno_column_name_data_type,
  janno_multi_value_columns,
  janno_choice_columns,
  janno_column_name_choices,
  janno_mandatory_columns,
  janno_unique_columns,
  janno_bonus_columns,
  janno_range_columns,
  janno_column_name_range_lower,
  janno_column_name_range_upper,
  internal = T, overwrite = T
)
