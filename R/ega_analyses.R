#' List All Analyses in EGA
#'
#' Get information about existing analyses in EGA. If provisional or permanent accession `analysis` id is specified, then only information about this asnalysis is shown.
#'
#' @param analysis Get information only for this analysis.
#' @param submission Get information for all analyses in this submission.
#'
#' @return tibble with information about analyses.
#' @export
#'
#' @examples \dontrun{
#' ega_analyses()
#' }
#'
ega_analyses <- function(submission=NULL, analysis=NULL) {
  # information about single analysis
  # analysis has preference over submission
  if (!is.null(analysis)) {
    ret <- ega_get(resource_prefix="analyses",
                   resource_id = analysis)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "analyses")
  } else {
    ret <- ega_get(resource_prefix="analyses")
  }
  return(ret)
}

#' Create Analysis in EGA
#'
#' Create analysis in EGA by assigning files to samples and experiments.
#'
#' @param submission Character scalar. Provisional or stable submission Id.
#' @param title Character scalar. The analysis title.
#' @param description Character scalar. The analysis description.
#' @param files List. Provisional Ids of the files.
#' @param analysis_type Character scalar. The analysis analysis type. See `ega_enums("analysis_types")`.
#' @param experiment_types Character scalar. Types of experiments.
#' @param genome_id Integer scalar. The analysis genome id. See `ega_enums("genomes")`.
#' @param chromosomes List. See `ega_enums("chromosomes")`.
#' @param platform Character scalar. The analysis platform.
#' @param study_provisional_id Integer scalar. The analysis's study's provisional ID.
#' @param study_accession_id Character scalar. The analysis's study's accession ID.
#' @param experiment_provisional_ids Integer scalar. The analysis's experiment's provisional ID.
#' @param experiment_accession_ids Character scalar. The analysis's experiment's accession ID.
#' @param sample_provisional_ids Integer vector. The analysis's sample's accession ID
#' @param sample_accession_ids Character scalar. The analysis's sample's accession ID.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble with information about the created analysis.
#' @export
#'
#' @examples \dontrun{
#' new_analysis <- ega_create_analysis(submission_id=provisional_id,
#' title="my first analysis", description="description of my first analysis",
#' files=c("75243", "75406"))
#' }
#'
ega_create_analysis <- function(submission,
                                title,
                                description,
                                analysis_type,
                                files,
                                experiment_types=NULL,
                                genome_id=NULL,
                                chromosomes=NULL,
                                platform=NULL,
                                study_provisional_id=NULL,
                                study_accession_id=NULL,
                                experiment_provisional_ids=NULL,
                                experiment_accession_ids=NULL,
                                sample_provisional_ids=NULL,
                                sample_accession_ids=NULL,
                                extra_attributes=NULL) {

  chromosomes <- check_list_str(chromosomes,
                            nms=c("id", "label"))
  extra_attributes <- check_list_str(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("submissions/",submission,"/analyses"),
                  method="POST",
                  title=title,
                  description=description,
                  analysis_type=analysis_type,
                  files=files,
                  experiment_types=experiment_types,
                  genome_id=genome_id,
                  chromosomes=chromosomes,
                  platform=platform,
                  study_provisional_id=study_provisional_id,
                  study_accession_id=study_accession_id,
                  experiment_provisional_ids=experiment_provisional_ids,
                  experiment_accession_ids=experiment_accession_ids,
                  sample_provisional_ids=sample_provisional_ids,
                  sample_accession_ids=sample_accession_ids,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Update Analysis In EGA
#'
#' Update information of this analysis.
#'
#' @param analysis character scalar. analysis Id.
#' @param title Character scalar. The analysis title.
#' @param description Character scalar. The analysis description.
#' @param files List. Provisional Ids of the files.
#' @param analysis_type Character scalar. The analysis analysis type. See `ega_enums("analysis_types")`.
#' @param experiment_types Character scalar. Types of experiments.
#' @param genome_id Integer scalar. The analysis genome id. See `ega_enums("genomes")`.
#' @param chromosomes List. See `ega_enums("chromosomes")`.
#' @param platform Character scalar. The analysis platform.
#' @param study_provisional_id Integer scalar. The analysis's study's provisional ID.
#' @param study_accession_id Character scalar. The analysis's study's accession ID.
#' @param experiment_provisional_ids Integer scalar. The analysis's experiment's provisional ID.
#' @param experiment_accession_ids Character scalar. The analysis's experiment's accession ID.
#' @param sample_provisional_ids Integer vector. The analysis's sample's accession ID
#' @param sample_accession_ids Character scalar. The analysis's sample's accession ID.
#' @param extra_attributes List. The extra attributes, list with element names: `tag`, `value` and `unit.`
##'
#' @return tibble. Information about updated analysis.
#' @export
#'
#' @examples \dontrun{
#' ega_update_analysis(analysis=1,
#' alias="My unique analysis alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_update_analysis <- function(analysis,
                                title,
                                description,
                                analysis_type,
                                files,
                                experiment_types=NULL,
                                genome_id=NULL,
                                chromosomes=NULL,
                                platform=NULL,
                                study_provisional_id=NULL,
                                study_accession_id=NULL,
                                experiment_provisional_ids=NULL,
                                experiment_accession_ids=NULL,
                                sample_provisional_ids=NULL,
                                sample_accession_ids=NULL,
                                extra_attributes=NULL) {

  chromosomes <- check_list_str(chromosomes,
                            nms=c("id", "label"))

  extra_attributes <- check_list_str(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("analyses/", analysis),
                  method="PUT",
                  title=title,
                  description=description,
                  analysis_type=analysis_type,
                  files=files,
                  experiment_types=experiment_types,
                  genome_id=genome_id,
                  chromosomes=chromosomes,
                  platform=platform,
                  study_provisional_id=study_provisional_id,
                  study_accession_id=study_accession_id,
                  experiment_provisional_ids=experiment_provisional_ids,
                  experiment_accession_ids=experiment_accession_ids,
                  sample_provisional_ids=sample_provisional_ids,
                  sample_accession_ids=sample_accession_ids,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete Run In EGA
#'
#' Delete this analysis.
#'
#' @param analysis character scalar. Provisional analysis Id.
#'
#' @return tibble. Information about deleted analysis.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_analysis(analysis=1)
#' }
ega_delete_analysis <- function(analysis) {
  resp <- req_ega(paste0("analyses/", analysis),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback Run In EGA
#'
#' Undo all the changes done to this analysis.
#'
#' @param analysis character scalar. Run accession Id.
#'
#' @return tibble. Information about rollback of the analysis.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_analysis(analysis=1)
#' }
ega_rollback_analysis <- function(analysis) {
  resp <- req_ega(paste0("analyses/", analysis, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
