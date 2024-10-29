#' List All Datasets in EGA
#'
#' Get information about existing datasets in EGA. If provisional or permanent accession `dataset` id is specified, then only information about this dataset is shown.
#'
#' @param dataset Get information only for this dataset.
#' @param submission Get information for all datasets in this submission.
#'
#' @return tibble with information about datasets.
#' @export
#'
#' @examples \dontrun{
#' ega_datasets()
#' }
#'
ega_datasets <- function(submission=NULL, dataset=NULL) {
  # information about single dataset
  # dataset has preference over submission
  if (!is.null(dataset)) {
    ret <- ega_get(resource_prefix="datasets",
                   resource_id = dataset)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "datasets")
  } else {
    ret <- ega_get(resource_prefix="datasets")
  }
  return(ret)
}

#' Create Dataset in EGA
#'
#' Create Dataset in EGA by assigning files to samples and experiments.
#'
#' @param submission Character scalar. Provisional or stable submission ID.
#' @param title Character scalar. Dataset title.
#' @param description  Character scalar. Dataset description.
#' @param dataset_types Character scalar. The dataset types. See `ega_enums("dataset_types")`.
#' @param policy_accession_id Character scalar. The policy accession ID.
#' @param run_provisional_ids Integer vector. Provisional Run IDs.
#' @param run_accession_ids Character vector. Accession Run IDs.
#' @param analysis_provisional_ids Integer vector. Provisional Analysis IDs.
#' @param analysis_accession_ids Character vector. Accession Analysis IDs.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble with information about the created dataset.
#' @export
#'
#' @examples \dontrun{
#' new_dataset <- ega_create_dataset(submission_id=provisional_id,
#' dataset_file_type="fastq", files=c("75243", "75406"),
#' experiment_provisional_id=7000, sample_provisional_id=46800)
#' }
#'
ega_create_dataset <- function(submission,
                           title, description,
                           dataset_types,
                           policy_accession_id,
                           run_provisional_ids=NULL,
                           run_accession_ids=NULL,
                           analysis_provisional_ids=NULL,
                           analysis_accession_ids=NULL,
                           extra_attributes=NULL) {

  extra_attributes <- assert_ega_list(extra_attributes,
                                 names=c("tag", "value", "unit"))
  resp <- req_ega(paste0("submissions/",submission,"/datasets"),
                  method="POST",
                  title=title, description=description,
                  dataset_types=dataset_types,
                  policy_accession_id=policy_accession_id,
                  run_provisional_ids=run_provisional_ids,
                  run_accession_ids=run_accession_ids,
                  analysis_provisional_ids=analysis_provisional_ids,
                  analysis_accession_ids=analysis_accession_ids,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Update Dataset In EGA
#'
#' Update information of this dataset.
#'
#' @param dataset character scalar. dataset Id.
#' @param title Character scalar. Dataset title.
#' @param description  Character scalar. Dataset description.
#' @param dataset_types Character scalar. The dataset types. See `ega_enums("dataset_types")`.
#' @param policy_accession_id Character scalar. The policy accession ID.
#' @param run_provisional_ids Integer vector. Provisional Run IDs.
#' @param run_accession_ids Character vector. Accession Run IDs.
#' @param analysis_provisional_ids Integer vector. Provisional Analysis IDs.
#' @param analysis_accession_ids Character vector. Accession Analysis IDs.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble. Information about updated dataset.
#' @export
#'
#' @examples \dontrun{
#' ega_update_dataset(dataset=1,
#' alias="My unique dataset alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_update_dataset <- function(dataset,
                               title, description,
                               dataset_types,
                               policy_accession_id,
                               run_provisional_ids=NULL,
                               run_accession_ids=NULL,
                               analysis_provisional_ids=NULL,
                               analysis_accession_ids=NULL,
                               extra_attributes=NULL) {

  extra_attributes <- assert_ega_list(extra_attributes,
                                 names=c("tag", "value", "unit"))
  resp <- req_ega(paste0("datasets/", dataset),
                  method="PUT",
                  title=title, description=description,
                  dataset_types=dataset_types,
                  policy_accession_id=policy_accession_id,
                  run_provisional_ids=run_provisional_ids,
                  run_accession_ids=run_accession_ids,
                  analysis_provisional_ids=analysis_provisional_ids,
                  analysis_accession_ids=analysis_accession_ids,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete Dataset In EGA
#'
#' Delete this dataset.
#'
#' @param dataset character scalar. Provisional dataset Id.
#'
#' @return tibble. Information about deleted dataset.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_dataset(dataset=1)
#' }
ega_delete_dataset <- function(dataset) {
  resp <- req_ega(paste0("datasets/", dataset),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback Dataset In EGA
#'
#' Undo all the changes done to this dataset.
#'
#' @param dataset character scalar. Dataset accession Id.
#'
#' @return tibble. Information about rollback of the dataset.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_dataset(dataset=1)
#' }
ega_rollback_dataset <- function(dataset) {
  resp <- req_ega(paste0("datasets/", dataset, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
