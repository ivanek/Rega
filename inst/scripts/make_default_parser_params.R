## Generate the default configuration for default_parser()
## Author: Igor Cervenka
## Licence: Artistic-2.0

default_params <- list(submission_sheets = c(
  "Aliases", "Files", "Analysis Files",
  "Submission", "Studies", "Samples", "Experiments", "Runs", "Analyses",
  "Datasets"
), linked_sheets = c(
  "Collaborators", "Repositories",
  "Extra Attributes"
), delimited_columns = list(names = c(
  "pubmed_ids",
  "custom_tags"
), separator = ";"), formatter = list(
  aliases = list(
    type = "aliases_formatter", params = list(na_omit = TRUE)
  ),
  select_input_data = list(type = "aliases_formatter", params = list(
    na_omit = TRUE
  )), files = list(
    type = "file_formatter",
    params = list(prefix = "", crypt_ext = "c4gh", prepend_slash = TRUE)
  ),
  analysis_files = list(type = "file_formatter", params = list(
    prefix = "", crypt_ext = "c4gh", prepend_slash = TRUE
  )),
  submission = list(type = "row_table_formatter", params = list(
    fold = "collaborators"
  )), studies = list(
    type = "row_table_formatter",
    params = list(fold = c("extra_attributes", "repositories"))
  ), experiments = list(
    type = "row_table_formatter",
    params = list(fold = "extra_attributes")
  ), samples = list(
    type = "column_table_formatter", params = FALSE
  ), runs = list(
    type = "column_table_formatter", params = list(fold = c(
      "files",
      "extra_attributes"
    ))
  ), analyses = list(
    type = "row_table_formatter",
    params = list(fold = c(
      "extra_attributes", "chromosomes",
      "experiments", "samples", "files", "experiment_types"
    ))
  ), datasets = list(type = "row_table_formatter", params = list(
    fold = c(
      "dataset_types", "extra_attributes", "runs",
      "analyses"
    )
  )), collaborators = list(
    type = "column_table_formatter",
    params = FALSE
  ), repositories = list(
    type = "column_table_formatter",
    params = FALSE
  ), extra_attributes = list(
    type = "column_table_formatter",
    params = FALSE
  )
))

yaml::write_yaml(
  default_params,
  file = file.path("..", "extdata", "default_parser_params.yaml")
)
