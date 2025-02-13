#' Retrieve Rega secret from Environment Variable
#'
#' Retrieves the Rega secret from the specified environment variable. If the key
#' is not found, a warning message is issued. REGA secret should be generated
#' via `httr2::secret_make_key()` and stored as environmental variable either by
#' using `export` command in bash or at the the user-level in the `.Renviron`
#' file
#'
#' @param envvar A string specifying the name of the environment variable to
#'   retrieve the API key from. Defaults to \code{"REGA_KEY"}.
#'
#' @return A string containing the REGA API key, or an empty string if the
#'   variable is not set.
#'
#' @importFrom httr2 secret_make_key
#'
#' @examples
#' rega_key <- Rega:::.get_rega_key()
#'
#' @keywords internal
.get_rega_key <- function(envvar = "REGA_KEY") {
    rega_key <- Sys.getenv(envvar)
    if (identical(rega_key, "")) {
        warn_msg <- paste(
            sprintf("No %s environmental variable found.", envvar),
            "Attempting to conect via unecrypted password."
        )
        warning(warn_msg)
    }
    return(rega_key)
}

#' Get EGA User Name
#'
#' Retrieves the EGA username from the specified environment variable. If the
#' variable is not set, prompts the user to enter their username interactively.
#'
#' @param envvar A string specifying the name of the environment variable to
#'   retrieve the username from. Defaults to \code{"REGA_EGA_USERNAME"}.
#'
#' @return A string containing the EGA username.
#'
#' @importFrom askpass askpass
#'
#' @examples
#' ega_username <- Rega:::.get_ega_username()
#' ega_username <- Rega:::.get_ega_username("EGA_USERNAME")
#'
#' @keywords internal
.get_ega_username <- function(envvar = "REGA_EGA_USERNAME") {
    ega_username <- Sys.getenv(envvar)
    if (identical(ega_username, "")) {
        ega_username <- askpass(prompt = "Please enter your EGA username:")
    }
    return(ega_username)
}

#' Get EGA User Password
#'
#' Retrieves the EGA password from the specified environment variable. If not
#' found, prompts the user to enter it. If an encryption key (usually stored in
#' REGA_KEY environmental variable) is available, the password is decrypted.
#'
#' @param envvar A string specifying the name of the environment variable to
#'   retrieve the password from. Defaults to \code{"REGA_EGA_PASSWORD"}.
#' @param ... Additional arguments passed to \code{.get_rega_key}.
#'
#' @return A string containing the decrypted EGA password.
#'
#' @importFrom askpass askpass
#' @importFrom httr2 secret_decrypt
#'
#' @examples
#' ega_password <- Rega:::.get_ega_password()
#'
#' @keywords internal
.get_ega_password <- function(envvar = "REGA_EGA_PASSWORD", ...) {
    rega_key <- .get_rega_key(...)
    ega_password <- Sys.getenv(envvar)

    if (!identical(ega_password, "") && identical(rega_key, "")) {
        warning("Storing unencrypted password in plaintext is not recommended.")
    }

    # Ask for password if not found in environmental variable
    if (identical(ega_password, "")) {
        ega_password <- askpass(prompt = "Please enter your EGA username:")
    }

    # If `rega_key` environmental variable secret is set, decrypt the password
    if (!identical(rega_key, "")) {
        ega_password <- secret_decrypt(Sys.getenv(envvar), I(rega_key))
    }

    return(ega_password)
}

#' Set The OAUTH With EGA Username And Password
#'
#' `ega_oauth` implements the EGA OAuth resource owner password flow, as defined
#' by Section 4.3 of RFC 6749. It allows the user to supply their password once,
#' exchanging it for an access token that can be cached locally. Please avoid
#' entering the password directly when calling this function as it will be
#' captured by `.Rhistory`.
#'
#' @param req A httr2 request.
#' @param username Character. EGA User name. Defaults to the value returned by
#'   `.get_ega_username()`.
#' @param password Character. EGA user Password. Defaults to the value returned
#'   by `.get_ega_password()`.
#' @param token_url Character. The URL for the EGA token endpoint. Defaults to
#'   the standard EGA token URL if not provided.
#'
#' @return returns a modified HTTP request that will use OAuth
#'
#' @importFrom httr2 oauth_client
#' @importFrom httr2 req_oauth_password
#'
#' @examples
#' req <- httr2::request("https://example.com/")
#'
#' # Request OAuth with default credentials
#' oauth_req <- ega_oauth(req)
#'
#' # Request OAuth with custom credentials
#' oauth_req <- ega_oauth(req, username = "user", password = "pass")
#'
#' @export
ega_oauth <- function(
    req, username = .get_ega_username(), password = .get_ega_password(),
    token_url = NULL) {
    if (is.null(token_url)) {
        token_url <- paste0(
            "https://idp.ega-archive.org/",
            "realms/EGA/protocol/openid-connect/token"
        )
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
#' This function retrieves an API token from the European Genome-phenome Archive
#' (EGA) using user credentials.
#'
#' @param username Character. The username for EGA authentication. Defaults to
#'   the value returned by `.get_ega_username()`.
#' @param password Character. The password for EGA authentication. Defaults to
#'   the value returned by `.get_ega_password()`.
#' @param token_url Character. The URL for the EGA token endpoint. Defaults to
#'   the standard EGA token URL if not provided.
#'
#' @return A list containing the token details if successful. Actual token value
#'   can be retrieved by `token$access_token`
#'
#' @importFrom httr2 request req_body_form req_perform resp_body_json
#'   resp_body_string
#'
#' @examples
#' try(
#'     ega_token(username = "my_username", password = "my_password")
#' )
#'
#' try(
#'     ega_token(token_url = "https://www.example.com")
#' )
#'
#' @export
ega_token <- function(
    username = .get_ega_username(), password = .get_ega_password(),
    token_url = NULL) {
    if (is.null(token_url)) {
        # Use default EGA token URL
        token_url <- paste0(
            "https://idp.ega-archive.org/",
            "realms/EGA/protocol/openid-connect/token"
        )
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
