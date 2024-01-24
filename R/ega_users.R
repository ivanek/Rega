#' Show User ID From EGA
#'
#' Show current user ID for the specified username.
#'
#' @param username Character scalar. EGA username.
#'
#' @return Tibble. Table with username and user ID.
#' @export
#'
#' @examples \dontrun{
#' ega_userid()
#' }
#'
ega_users <- function(username=ega_get_username()) {
  ret <- ega_get(resource_prefix=paste0("users/", username))
  return(ret)
}
