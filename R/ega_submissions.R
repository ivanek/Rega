#' List All Submissions In EGA
#'
#' Get information about existing submissions in EGA for the current user. If provisional or permanent accession `submission` id is specified, then only information about this submission is shown.
#'
#' @param submission Get information only for this submission.
#'
#' @importFrom checkmate assert_string
#'
#' @return tibble with information about submission(s).
#' @export
#'
#' @examples \dontrun{
#' ega_submissions()
#' }
#'
ega_submissions <- function(submission=NULL) {
    submission <- assert_ega_id(submission)
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission)
  return(ret)
}



#' Create A New Submission In EGA
#'
#' @param title character scalar. Submission title.
#' @param description  character scalar. Submission description.
#' @param collaborators list. Default `NULL` otherwise `list` with three
#' elements: `id` containing user id; `access_type` granted either "read"
#' or "write"; `comment` to collaborator.
#'
#' @importFrom checkmate assert_string
#'
#' @return tibble. Information about newly created submission.
#' @export
#'
#' @examples \dontrun{
#' ega_create_submission(title="Submission Title",
#' description="Submission description")
#' }
ega_create_submission <- function(title, description, collaborators=NULL) {

  title <- checkmate::assert_string(title)
  description <- checkmate::assert_string(description)
  collaborators <- assert_ega_list(collaborators,
                              names=c("id", "access_type", "comment"),
                              types=c("integer", "character", "character"))

  resp <- req_ega("submissions",
                  method="POST",
                  title=title,
                  description=description,
                  collaborators=collaborators)
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Update Submission In EGA
#'
#' Update information of this submission.
#'
#' @param submission character scalar. Submission Id.
#' @param title character scalar. Submission title.
#' @param description  character scalar. Submission description.
#'
#' @importFrom checkmate assert_string
#'
#' @return tibble. Information about updated submission.
#' @export
#'
#' @examples \dontrun{
#' ega_update_submission(submission=1, title="Submission Title (update)",
#' description="Submission description (update)")
#' }
ega_update_submission <- function(submission, title, description) {

  submission <- assert_ega_id(submission)
  title <- checkmate::assert_string(title)
  description <- checkmate::assert_string(description)

  resp <- req_ega(paste0("submissions/", submission),
                  method="PUT",
                  title=title,
                  description=description)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete Submission In EGA
#'
#' Delete this submission, including all the objects in it.
#'
#' @param submission character scalar. Submission Id.
#'
#' @return tibble. Information about deleted submission.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_submission(submission=1)
#' }
ega_delete_submission <- function(submission) {

  submission <- assert_ega_id(submission)

  resp <- req_ega(paste0("submissions/", submission),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback Submission In EGA
#'
#' Undo all the changes done in all the objects in this submission which have been modified, including the submission itself, and delete any new object that has been created.
#'
#' @param submission character scalar. Submission Id.
#'
#' @return tibble. Information about rollback of the submission.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_submission(submission=1)
#' }
ega_rollback_submission <- function(submission) {

  submission <- assert_ega_id(submission)

  resp <- req_ega(paste0("submissions/", submission, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Finalise The Submission In EGA
#'
#' Finalise this submission.
#'
#' @param submission character scalar. Submission Id.
#' @param expected_release_date character scalar. Expected release date in the format YYYY-MM-DD.
#' @param dataset_changelogs list. List with two elements: `dataset` with dataset ids and `message` change log messages.
#'
#' @return tibble. Information about finalised submission.
#' @export
#'
#' @examples \dontrun{
#' ega_finalise_submission(submission=1)
#' }
ega_finalise_submission <- function(submission,
                                    expected_release_date = Sys.Date() + 365,
                                    dataset_changelogs=NULL) {

  submission <- assert_ega_id(submission)
  expected_release_date <- checkmate::check_date(expected_release_date, len=1)
  dataset_changelogs <- assert_ega_list(dataset_changelogs,
                              names=c("dataset", "message"),
                              types=c("character", "character"))

  resp <- req_ega(paste0("submissions/", submission, "/finalise"),
                  expected_release_date = expected_release_date,
                  dataset_changelogs = dataset_changelogs,
                  method="POST")
  resp <- ega_parse_body(resp)
  return(resp)
}
