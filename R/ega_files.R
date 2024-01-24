#' Show Information About Existing Files In EGA
#'
#' Show information about all the files for the current EGA user.
#'
#' @param submission Character scalar. Get information for all files
#' in this submission ID.
#' @param prefix Character scalar. Show files matching this prefix.
#' @param status Character scalar. Show files matching this status.
#'
#' @return Tibble. Table with information about the files.
#' @export
#'
#' @examples \dontrun{
#' ega_files()
#' }
ega_files <- function(submission=NULL, prefix=NULL, status=NULL) {
  query <- ""
  if (!is.null(prefix))
    query <- paste0(query, "?prefix=", prefix)
  if (!is.null(status))
    if (query == "") {
      query <- paste0(query, "?status=", status)
    } else {
      query <- paste0(query, "&status=", status)
    }

  if (!is.null(submission)) {
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = paste0("files", query))
  } else {
    ret <- ega_get(resource_prefix= paste0("files", query))
  }
  return(ret)
}
