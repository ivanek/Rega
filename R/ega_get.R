#' Parse The Information From httr2 Response Object.
#'
#' Parse the information from the body of `httr2` response object and returns it as a `tibble`.
#'
#' @param resp httr2 response object.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_string
#' @importFrom httr2 resp_content_type
#' @importFrom jsonlite fromJSON
#' @importFrom rlang :=
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#'
#' @return tibble with obtained information.
#' @keywords internal
#'
#' @examples \dontrun{
#' ega_parse_body(resp)
#' }
ega_parse_body <- function(resp) {

  resource_name <- resp |>
    httr2::resp_url_path() |>
    str_replace("\\/api\\/(\\w+)\\/?.*", "\\1")

  if (httr2::resp_content_type(resp) == "application/json") {
    # check if the content is JSON
    resp <- resp |>
      httr2::resp_body_json()

    # special treatment for short list (e.g. user info)
    if(!is.null(names(resp))) {
      resp <- resp |>
        list()
    }
  } else if (httr2::resp_content_type(resp) == "text/plain") {
    # if plain text, convert to JSON list
    resp <- resp |>
      httr2::resp_body_string()
    if (grepl("^\\{.*\\}$", resp)) {
      # if there is JSON like structure
      resp <- resp |>
        jsonlite::fromJSON() |>
        lapply(function(x) if (is.null(x)) list() else x) |>
        list()
    } else {
      # in case there is no JSON like structure
      # return the value as an one-column tibble
      return(tibble::tibble("{resource_name}" := resp))
    }
  }

  resp <- resp |>
    tibble::tibble() |>
    dplyr::rename("{resource_name}" := 1) |>
    tidyr::unnest_wider({resource_name}, names_sep = "/",
                        names_repair="unique") # in the datasets response, there are 2 columns with status, why?
  if(ncol(resp) == 1) {
    resp <- resp |>
      dplyr::rename("{resource_name}" := 1)
  }
  # remove anything before slash .*/from column names
  resp <- resp |>
    dplyr::rename_with( ~ str_replace(.x, ".*\\/", ""))
  return(resp)
}

#' Main EGA Get Function
#'
#' @param resource_prefix character scalar. First part of API url
#' @param resource_id character scalar. EGA object ID
#' @param resource_suffix character scalar.
#'
#' @importFrom checkmate assert_string
#'
#' @return tibble with obtained information.
#' @keywords internal
#'
#'
#' @examples \dontrun{
#' ega_get(resource_prefix="enums")
#' }
#'
ega_get <- function(resource_prefix, resource_id=NULL, resource_suffix=NULL) {
  resource <- assert_string(resource_prefix)
  if (!is.null(resource_id)) {
    resource_id <- assert_string(resource_id)
    resource <- paste0(resource, "/", resource_id)
  }

  if (!is.null(resource_suffix) && !is.null(resource_id)) {
    resource_suffix <- assert_string(resource_suffix)
    resource <- paste0(resource, "/", resource_suffix)
  }

  resp <- req_ega(resource=resource)
  resp <- resp |>
    ega_parse_body()
  return(resp)
}
