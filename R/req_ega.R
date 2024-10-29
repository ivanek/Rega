#' Generate Rega Key
#'
#' `set_rega_key` generates an encryption key and set it as an ENV variable
#' "REGA_KEY". Alternative might be to set permanent key on the user-level in
#' the `.Renviron` (which can be easily open with `usethis::edit_r_environ())`
#'
#' @return NULL
#'
#' @keywords internal
#'
#' @importFrom httr2 secret_make_key
#' @examples
#' Rega:::set_rega_key()
#'
set_rega_key <- function() {
  rega_key <- Sys.getenv("REGA_KEY")
  if (identical(rega_key, "")) {
    Sys.setenv("REGA_KEY" = httr2::secret_make_key())
  }
}

#' Get EGA Username
#'
#' `ega_get_username` get EGA username either from ENV variable "REGA_EGA_USERNAME" or ask the user to provide it.
#'
#'
#' @return character scalar with EGA username
#'
#' @keywords internal
#'
#' @importFrom askpass askpass
#'
#' @examples
#' ega_get_username <- Rega:::ega_get_username()
#'
ega_get_username <- function() {
  ega_username <- Sys.getenv("REGA_EGA_USERNAME")
  if (identical(ega_username, "")) {
    ega_username <- askpass::askpass(prompt = "Please enter your EGA username:")
  }
  if (!is.null(ega_username)) {
    Sys.setenv("REGA_EGA_USERNAME" = ega_username)
  }
  return(ega_username)
}


#' Get EGA User Password
#'
#' `ega_get_password` get EGA password either from ENV variable "REGA_EGA_PASSWORD" or ask the user to provide it.
#' The encrypted password is stored in the ENV variable "REGA_EGA_PASSWORD". ENV variable "REGA_KEY" is used to en-/de-crypt the password.
#'
#'
#' @return character scalar with EGA password
#'
#' @keywords internal
#'
#' @importFrom httr2 secret_decrypt
#' @importFrom httr2 secret_encrypt
#' @importFrom askpass askpass
#'
#' @examples
#' ega_get_password <- Rega:::ega_get_password()
#'
ega_get_password <- function() {
  set_rega_key()
  ega_password <- httr2::secret_decrypt(Sys.getenv("REGA_EGA_PASSWORD"), "REGA_KEY")
  if (identical(ega_password, "")) {
    ega_password <- askpass::askpass(prompt="Please enter your EGA password:")
    if (!is.null(ega_password)) {
      Sys.setenv("REGA_EGA_PASSWORD" = httr2::secret_encrypt(ega_password, "REGA_KEY"))
    }
  }
  return(ega_password)
}


#' Set The OAUTH With EGA Username And Password
#'
#' `req_ega_auth` implements the EGA OAuth resource owner password flow, as defined by Section 4.3 of RFC 6749.
#' It allows the user to supply their password once, exchanging it for an access token that can be cached locally.
#'
#' @param req A httr2 request.
#' @param ega_username Character scalar. EGA User name. By default the content of ENV variable "REGA_EGA_USERNAME".
#' @param ega_password Character scalar.EGA user Password. By default the content of ENV variable "REGA_EGA_PASSWORD". Please avoid entering the password directly when calling this function as it will be captured by `.Rhistory`.
#'
#' @return returns a modified HTTP request that will use OAuth;
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom httr2 oauth_client
#' @importFrom httr2 req_oauth_password
#' @examples \dontrun{
#' Rega:::req_ega_auth(req)
#' }
#'
req_ega_auth <- function(req, ega_username=ega_get_username(),
                         ega_password=ega_get_password()) {

  ega_client <- httr2::oauth_client(
    id = "sp-api",
    token_url = "https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token",
    name = "Rega"
  )

  httr2::req_oauth_password(req,
                            client = ega_client,
                            username = ega_username,
                            password = ega_password,
                            cache_disk = TRUE,
                            cache_key = ega_username)
}


#' Print-out API Error Messages
#'
#' @param resp `httr2_response` object
#'
#' @return Character Scalar. EGA API Error Message.
#' @keywords internal
#' @noRd
#'
#' @importFrom httr2 resp_body_string
#' @importFrom stringr str_replace
#'
#' @examples \dontrun{
#' resp <- last_response()
#' resp |> ega_error_body()
#' }
ega_error_body <- function(resp) {
  resp |>
    httr2::resp_body_string() |>
    stringr::str_replace("^\\d+: ", "")
}

#' Send Request to EGA API
#'
#' `req_ega` sends an request to EGA API.
#'
#' @param resource Character scalar. Specific resource to be queried.
#' @param ... named parameters which can be send with POST method as a body of httr2 request (in a JSON format)
#' @param method Character scalar. HTTP method like "GET", "POST", "DELETE"
#'
#' @return An HTTP response from EGA API: an S3 list with class `httr2_request`.
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_url_path_append
#' @importFrom httr2 req_user_agent
#' @importFrom httr2 req_method
#' @importFrom httr2 req_method
#' @importFrom httr2 req_body_json
#' @importFrom httr2 req_error
#' @importFrom httr2 req_perform
#'
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' Rega:::req_ega("enums")
#'}
#'
req_ega <- function(resource, ..., method="GET") {
  params <- list(
    ...
  )

  req <- httr2::request("https://submission.ega-archive.org/api") |>
    httr2::req_url_path_append(resource) |>
    httr2::req_user_agent("Rega (https://github.com/ivanek/Rega)") |>
    req_ega_auth() |>
    httr2::req_method(method=method)

  if (!is.null(params)) {
    req <- req |>
      httr2::req_body_json(data=params)
  }

  req <- req |>
    httr2::req_error(body = ega_error_body) |>
    httr2::req_perform()
}

