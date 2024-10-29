#' Show Predefined Values From EGA
#'
#' Show predefined values from EGA.
#'
#' @param enums Character scalar. Show predefined values for the specified
#' enumeration. Default `NULL` shows all enumerations available.
#'
#' @importFrom checkmate assert_string
#'
#' @return Tibble. Table with predefined values.
#' @export
#'
#' @examples \dontrun{
#' ega_enums()
#' }
ega_enums <- function(enums = NULL) {
  resource <- "enums"
  if (!is.null(enums)) {
    enums <- checkmate::assert_string(enums)
    resource <- paste0(resource, "/", enums)
  }
  ret <- ega_get(resource_prefix=resource)
  return(ret)
}
