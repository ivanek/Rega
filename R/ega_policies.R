#' List All Polices In EGA
#'
#' Get information about existing policies. If `DAC` is specified, then only
#' policies corresponding to the specific Data Access Committee (DAC) are shown.
#'
#' @param dac Character scalar. Show existing polices only for this DAC.
#'
#' @importFrom checkmate assert_string
#'
#' @return Tibble. Table with showing existing polices.
#' @export
#'
#' @examples \dontrun{
#' ega_policies()
#' }
ega_polices <- function(dac=NULL) {
  if (!is.null(dac)) {
    dac <- checkmate::assert_string(dac)
    ret <- ega_get(resource_prefix="dacs",
                   resource_id = dac,
                   resource_suffix = "policies")
  } else {
    ret <- ega_get(resource_prefix="policies")
  }
  return(ret)
}
