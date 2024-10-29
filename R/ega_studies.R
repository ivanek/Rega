#' List All Studies in EGA
#'
#' Get information about existing studies in EGA. If provisional or permanent accession `study` id is specified, then only information about this study is shown.
#'
#' @param study Get information only for this study.
#' @param submission Get information for all studies in this submission.
#'
#' @return tibble with information about studies.
#' @export
#'
#' @examples \dontrun{
#' ega_studies()
#' }
#'
ega_studies <- function(submission=NULL, study=NULL) {
  # information about single study
  # study has preference over submission
  if (!is.null(study)) {
    study <- assert_ega_id(study)
    ret <- ega_get(resource_prefix="studies",
                   resource_id = study)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    submission <- assert_ega_id(submission)
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "studies")
  } else {
    ret <- ega_get(resource_prefix="studies")
  }
  return(ret)
}


#' Create A New study In EGA
#'
#' @param submission character scalar. Submission Id. Study will be created within this submission.
#' @param title character scalar. Study title.
#' @param description  character scalar. Study description.
#' @param study_type Study type, for options check output of `ega_enums(enums="study_types")`.
#' @param pubmed_ids list. List study PubMedIDs. Single element list without names.
#' @param custom_tags list. The study custom tags. Single element list without names.
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`
#' @param repositories list. The external references to other repositories, list with element names: `repository_id`, `url` and `label`.
#'
#' @importFrom checkmate assert_string
#'
#' @return tibble. Information about newly created study.
#' @export
#'
#' @examples \dontrun{
#' ega_create_study(title="study Title",
#' description="study description")
#' }
ega_create_study <- function(submission, title, description, study_type,
                             pubmed_ids=NULL, custom_tags=NULL,
                             extra_attributes=NULL,
                             repositories=NULL) {

  submission <- assert_ega_id(submission)
  title <- checkmate::assert_string(title)
  description <- checkmate::assert_string(description)
  study_type <- checkmate::assert_string(study_type)

  # pubmed_ids <- checkmate::assert_list()

  pubmed_ids <- assert_ega_list(pubmed_ids)
  custom_tags <- assert_ega_list(custom_tags, types="character")
  extra_attributes <- assert_ega_list(extra_attributes,
                              names=c("tag", "value", "unit"))
  repositories <- assert_ega_list(repositories,
                                 names=c("repository_id", "url", "label"))
  resp <- req_ega(paste0("submissions/", submission, "/studies"),
                  method="POST",
                  title=title,
                  description=description,
                  study_type=study_type,
                  pubmed_ids=pubmed_ids,
                  custom_tags=custom_tags,
                  extra_attributes=extra_attributes,
                  repositories=repositories)
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Update study In EGA
#'
#' Update information of this study.
#'
#' @param study character scalar. study Id.
#' @param title character scalar. study title.
#' @param description  character scalar. study description.
#' @param study_type Study type, for options check output of `ega_enums(enums="study_types")`.
#' @param pubmed_ids list. List study PubMedIDs. Single element list without names.
#' @param custom_tags list. The study custom tags. Single element list without names.
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`
#' @param repositories list. The external references to other repositories, list with element names: `repository_id`, `url` and `label`.
#'
#' @return tibble. Information about updated study.
#' @export
#'
#' @examples \dontrun{
#' ega_update_study(study=1, title="study Title (update)",
#' description="study description (update)")
#' }
ega_update_study <- function(study, title, description, study_type,
                             pubmed_ids=NULL, custom_tags=NULL,
                             extra_attributes=NULL,
                             repositories=NULL) {
  pubmed_ids <- assert_ega_list(pubmed_ids, names="")
  custom_tags <- assert_ega_list(custom_tags, names="")
  extra_attributes <- assert_ega_list(extra_attributes,
                                 names=c("tag", "value", "unit"))
  repositories <- assert_ega_list(repositories,
                             names=c("repository_id", "url", "label"))
  resp <- req_ega(paste0("studies/", study),
                  method="PUT",
                  title=title,
                  description=description,
                  study_type=study_type,
                  pubmed_ids=pubmed_ids,
                  custom_tags=custom_tags,
                  extra_attributes=extra_attributes,
                  repositories=repositories)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete study In EGA
#'
#' Delete this study, including all the objects in it.
#'
#' @param study character scalar. Provisional study Id.
#'
#' @return tibble. Information about deleted study.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_study(study=1)
#' }
ega_delete_study <- function(study) {
  resp <- req_ega(paste0("studies/", study),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback study In EGA
#'
#' Undo all the changes done in all the objects in this study which have been modified, including the study itself, and delete any new object that has been created.
#'
#' @param study character scalar. Study accession Id.
#'
#' @return tibble. Information about rollback of the study.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_study(study=1)
#' }
ega_rollback_study <- function(study) {
  resp <- req_ega(paste0("studies/", study, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
