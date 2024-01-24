#' List All Experiments in EGA
#'
#' Get information about existing experiments in EGA. If provisional or permanent accession `experiment` id is specified, then only information about this experiment is shown.
#'
#' @param experiment Get information only for this experiment.
#' @param submission Get information for all experiments in this submission.
#'
#' @return tibble with information about experiments.
#' @export
#'
#' @examples \dontrun{
#' ega_experiments()
#' }
#'
ega_experiments <- function(submission=NULL, experiment=NULL) {
  # information about single experiment
  # experiment has preference over submission
  if (!is.null(experiment)) {
    ret <- ega_get(resource_prefix="experiments",
                   resource_id = experiment)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "experiments")
  } else {
    ret <- ega_get(resource_prefix="experiments")
  }
  return(ret)
}


#' Create experiment In EGA
#'
#' Create a single experiment in EGA.
#'
#' @param submission character scalar. Provisional or stable submission id.
#' @param study_provisional_id  character scalar. The experiment's study provisional ID. Use this field if the referenced study does not have an accession
#' @param study_accession_id character scalar. The experiment's study accession ID. Use this field if the referenced study already has an accession
#' @param design_description character scalar. The experiment alias.
#' @param library_name character scalar. The experiment title.
#' @param library_construction_protocol character scalar.
#' @param paired_nominal_length integer scalar. The experiment paired nominal length.
#' @param paired_nominal_sdev numeric scalar. The experiment paired nominal sdev.
#' @param instrument_model_id character scalar. The experiment instrument model id. See `ega_enums("platform_models")`.
#' @param library_layout character scalar. The experiment library layout. See `ega_enums("library_layouts")`.
#' @param library_strategy character scalar. The experiment library strategy. See `ega_enums("library_strategies")`.
#' @param library_source character scalar. The experiment library source. See `ega_enums("library_sources")`.
#' @param library_selection character scalar. The experiment library selection. See `ega_enums("library_selections")`.
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return `tibble` with information about the created experiment.
#' @export
#'
#' @examples \dontrun{
#' new_experiment <- ega_create_experiment(submission=provisional_id,
#' alias="My unique experiment alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_create_experiment <- function(submission,
                                  study_provisional_id,
                                  study_accession_id=NULL,
                                  design_description,
                                  library_name=NULL,
                                  library_construction_protocol=NULL,
                                  paired_nominal_length=NULL,
                                  paired_nominal_sdev=NULL,
                                  instrument_model_id,
                                  library_layout,
                                  library_strategy,
                                  library_source,
                                  library_selection,
                                  extra_attributes=NULL) {

  extra_attributes <- check_list(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("submissions/",submission,"/experiments"),
                  method="POST",
                  study_provisional_id=study_provisional_id,
                  study_accession_id=study_accession_id,
                  design_description=design_description,
                  library_name=library_name,
                  library_construction_protocol=library_construction_protocol,
                  paired_nominal_length=paired_nominal_length,
                  paired_nominal_sdev=paired_nominal_sdev,
                  instrument_model_id=instrument_model_id,
                  library_layout=library_layout,
                  library_strategy=library_strategy,
                  library_source=library_source,
                  library_selection=library_selection,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Update experiment In EGA
#'
#' Update information of this experiment
#'
#' @param experiment character scalar. experiment Id.
#' @param study_provisional_id  character scalar. The experiment's study provisional ID. Use this field if the referenced study does not have an accession
#' @param study_accession_id character scalar. The experiment's study accession ID. Use this field if the referenced study already has an accession
#' @param design_description character scalar. The experiment alias.
#' @param library_name character scalar. The experiment title.
#' @param library_construction_protocol character scalar.
#' @param paired_nominal_length integer scalar. The experiment paired nominal length.
#' @param paired_nominal_sdev numeric scalar. The experiment paired nominal sdev.
#' @param instrument_model_id character scalar. The experiment instrument model id. See `ega_enums("platform_models")`.
#' @param library_layout character scalar. The experiment library layout. See `ega_enums("library_layouts")`.
#' @param library_strategy character scalar. The experiment library strategy. See `ega_enums("library_strategies")`.
#' @param library_source character scalar. The experiment library source. See `ega_enums("library_sources")`.
#' @param library_selection character scalar. The experiment library selection. See `ega_enums("library_selections")`.
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`

#' @return tibble. Information about updated experiment.
#' @export
#'
#' @examples \dontrun{
#' ega_update_experiment(experiment=1,
#' alias="My unique experiment alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_update_experiment <- function(experiment,
                                  design_description,
                                  study_provisional_id,
                                  study_accession_id=NULL,
                                  library_name=NULL,
                                  library_construction_protocol=NULL,
                                  paired_nominal_length=NULL,
                                  paired_nominal_sdev=NULL,
                                  instrument_model_id,
                                  library_layout,
                                  library_strategy,
                                  library_source,
                                  library_selection,
                                  extra_attributes=NULL) {

  extra_attributes <- check_list(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("experiments/", experiment),
                  method="PUT",
                  study_provisional_id=study_provisional_id,
                  study_accession_id=study_accession_id,
                  design_description=design_description,
                  library_name=library_name,
                  library_construction_protocol=library_construction_protocol,
                  paired_nominal_length=paired_nominal_length,
                  paired_nominal_sdev=paired_nominal_sdev,
                  instrument_model_id=instrument_model_id,
                  library_layout=library_layout,
                  library_strategy=library_strategy,
                  library_source=library_source,
                  library_selection=library_selection,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete experiment In EGA
#'
#' Delete this experiment.
#'
#' @param experiment character scalar. Provisional experiment Id.
#'
#' @return tibble. Information about deleted experiment.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_experiment(experiment=1)
#' }
ega_delete_experiment <- function(experiment) {
  resp <- req_ega(paste0("experiments/", experiment),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback experiment In EGA
#'
#' Undo all the changes done to this experiment.
#'
#' @param experiment character scalar. experiment accession Id.
#'
#' @return tibble. Information about rollback of the experiment.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_experiment(experiment=1)
#' }
ega_rollback_experiment <- function(experiment) {
  resp <- req_ega(paste0("experiments/", experiment, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
