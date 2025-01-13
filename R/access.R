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
    Sys.setenv("REGA_KEY" = secret_make_key())
  }
}

#' Get EGA Username
#'
#' `get_ega_username` get EGA username either from ENV variable
#' "REGA_EGA_USERNAME" or ask the user to provide it.
#'
#'
#' @return character scalar with EGA username
#'
#' @keywords internal
#'
#' @importFrom askpass askpass
#'
#' @examples
#' ega_get_username <- Rega:::get_ega_username()
#'
get_ega_username <- function() {
  ega_username <- Sys.getenv("REGA_EGA_USERNAME")
  if (identical(ega_username, "")) {
    ega_username <- askpass(prompt = "Please enter your EGA username:")
  }
  if (!is.null(ega_username)) {
    Sys.setenv("REGA_EGA_USERNAME" = ega_username)
  }
  return(ega_username)
}

#' Get EGA User Password
#'
#' `get_ega_password` get EGA password either from ENV variable "REGA_EGA_PASSWORD"
#' or ask the user to provide it. The encrypted password is stored in the ENV
#' variable "REGA_EGA_PASSWORD". ENV variable "REGA_KEY" is used to en-/de-crypt
#' the password.
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
#' ega_get_password <- Rega:::get_ega_password()
#'
get_ega_password <- function() {
  set_rega_key()
  ega_password <- secret_decrypt(Sys.getenv("REGA_EGA_PASSWORD"), "REGA_KEY")
  if (identical(ega_password, "")) {
    ega_password <- askpass(prompt = "Please enter your EGA password:")
    if (!is.null(ega_password)) {
      Sys.setenv("REGA_EGA_PASSWORD" = secret_encrypt(ega_password, "REGA_KEY"))
    }
  }
  return(ega_password)
}

#' Set The OAUTH With EGA Username And Password
#'
#' `ega_oauth` implements the EGA OAuth resource owner password flow, as
#' defined by Section 4.3 of RFC 6749. It allows the user to supply their
#' password once, exchanging it for an access token that can be cached locally.
#'
#' @param req A httr2 request.
#' @param ega_username Character scalar. EGA User name. By default the content
#' of ENV variable "REGA_EGA_USERNAME".
#' @param ega_password Character scalar. EGA user Password. By default the
#' content of ENV variable "REGA_EGA_PASSWORD". Please avoid entering the
#' password directly when calling this function as it will be captured by
#' `.Rhistory`.
#'
#' @return returns a modified HTTP request that will use OAuth;
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom httr2 oauth_client
#' @importFrom httr2 req_oauth_password
#' @examples \dontrun{
#' Rega:::ega_oauth(req)
#' }
#'
ega_oauth <- function(req, username = get_ega_username(),
                      password = get_ega_password(), token_url = NULL) {
  if (is.null(token_url)) {
    token_url <- "https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token"
  }

  client <- oauth_client(
    id = "sp-api",
    token_url = token_url,
    name = "Rega"
  )

  request <- req_oauth_password(
    req,
    client = client,
    username = username,
    password = password,
    cache_disk = TRUE,
    cache_key = username
  )

  return(request)
}

#' Retrieve EGA API Bearer Token
#'
#' This function retrieves an API token from the European Genome-phenome
#' Archive (EGA) using user credentials.
#'
#' @param username Character. The username for EGA authentication.
#'   Defaults to the value returned by `get_ega_username()`.
#' @param password Character. The password for EGA authentication.
#' Defaults to the value returned by `get_ega_password()`.
#' @param token_url Character. The URL for the EGA token endpoint.
#' Defaults to the standard EGA token URL if not provided.
#'
#' @return A list containing the token details if successful. Actual token value
#'  can be retrieved by `token$access_token`
#'
#' @importFrom httr2 request req_body_form req_perform resp_body_json resp_body_string
#'
#' @examples
#' \dontrun{
#' # Retrieve token with default credentials and URL
#' token <- ega_token()
#'
#' # Retrieve token with custom credentials
#' token <- ega_token(username = "my_username", password = "my_password")
#'
#' # Retrieve token with a custom URL
#' token <- ega_token(token_url = "https://custom-token-url.example.com")
#' }
#'
#' @export
ega_token <- function(username = get_ega_username(),
                      password = get_ega_password(), token_url = NULL) {
  if (is.null(token_url)) {
    # Use default EGA token URL
    token_url <- "https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token"
  }

  response <- request(token_url) |>
    req_body_form(
      grant_type = "password",
      client_id = "sp-api",
      username = username,
      password = password
    ) |>
    req_perform()

  if (response$status_code == 200) {
    content <- resp_body_json(response)
  } else {
    content <- resp_body_string(response)
    message("Failed to obtain token: ", response$status_code)
  }

  return(content)
}
