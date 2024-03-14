#' List All Runs in EGA
#'
#' Get information about existing runs in EGA. If provisional or permanent accession `run` id is specified, then only information about this run is shown.
#'
#' @param run Get information only for this run.
#' @param submission Get information for all runs in this submission.
#'
#' @return tibble with information about runs.
#' @export
#'
#' @examples \dontrun{
#' ega_runs()
#' }
#'
ega_runs <- function(submission=NULL, run=NULL) {
  # information about single run
  # run has preference over submission
  if (!is.null(run)) {
    ret <- ega_get(resource_prefix="runs",
                   resource_id = run)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "runs")
  } else {
    ret <- ega_get(resource_prefix="runs")
  }
  return(ret)
}


#' Create Run in EGA
#'
#' Create Run in EGA by assigning files to samples and experiments.
#'
#' @param submission Character scalar. Provisional or stable submission Id.
#' @param run_file_type Character scalar. File type.
#' @param files List. Provisional Ids of the files.
#' @param experiment_provisional_id Character scalar. Provisional Experiment Id.
#' @param experiment_accession_id Character scalar. Experiment Accession Id.
#' @param sample_provisional_id Character scalar. Provisional Sample Id.
#' @param sample_accession_id character scalar. Sample Accession Id.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble with information about the created run.
#' @export
#'
#' @examples \dontrun{
#' new_run <- ega_create_run(submission_id=provisional_id,
#' run_file_type="fastq", files=c("75243", "75406"),
#' experiment_provisional_id=7000, sample_provisional_id=46800)
#' }
#'
ega_create_run <- function(submission,
                           run_file_type, files,
                           experiment_provisional_id=NULL,
                           experiment_accession_id=NULL,
                           sample_provisional_id=NULL,
                           sample_accession_id=NULL,
                           extra_attributes=NULL) {

  extra_attributes <- check_list_str(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("submissions/",submission,"/runs"),
                  method="POST",
                  run_file_type=run_file_type,
                  files=files,
                  experiment_provisional_id=experiment_provisional_id,
                  experiment_accession_id=experiment_accession_id,
                  sample_provisional_id=sample_provisional_id,
                  sample_accession_id=sample_accession_id,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Update Run In EGA
#'
#' Update information of this run.
#'
#' @param run character scalar. run Id.
#' @param run_file_type Character scalar. File type.
#' @param files List. Provisional Ids of the files.
#' @param experiment_provisional_id Character scalar. Provisional Experiment Id.
#' @param experiment_accession_id Character scalar. Experiment Accession Id.
#' @param sample_provisional_id Character scalar. Provisional Sample Id.
#' @param sample_accession_id character scalar. Sample Accession Id.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble. Information about updated run.
#' @export
#'
#' @examples \dontrun{
#' ega_update_run(run=1,
#' alias="My unique run alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_update_run <- function(run,
                           run_file_type, files,
                           experiment_provisional_id=NULL,
                           experiment_accession_id=NULL,
                           sample_provisional_id=NULL,
                           sample_accession_id=NULL,
                           extra_attributes=NULL) {

  extra_attributes <- check_list_str(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("runs/", run),
                  method="PUT",
                  run_file_type=run_file_type, files=files,
                  experiment_provisional_id=experiment_provisional_id,
                  experiment_accession_id=experiment_accession_id,
                  sample_provisional_id=sample_provisional_id,
                  sample_accession_id=sample_accession_id,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete Run In EGA
#'
#' Delete this run.
#'
#' @param run character scalar. Provisional run Id.
#'
#' @return tibble. Information about deleted run.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_run(run=1)
#' }
ega_delete_run <- function(run) {
  resp <- req_ega(paste0("runs/", run),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback Run In EGA
#'
#' Undo all the changes done to this run.
#'
#' @param run character scalar. Run accession Id.
#'
#' @return tibble. Information about rollback of the run.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_run(run=1)
#' }
ega_rollback_run <- function(run) {
  resp <- req_ega(paste0("runs/", run, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
