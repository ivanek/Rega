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
.get_rega_key <- function(secret_ennvar = "REGA_KEY") {
    .validate_character_scalar(secret_ennvar)

    rega_key <- Sys.getenv(secret_ennvar)
    if (identical(rega_key, "")) {
        warn_msg <- paste(
            sprintf("No %s environmental variable found.", secret_ennvar),
            "Using unecrypted password is not permitted."
        )
        stop(warn_msg)
    }
    rega_key
}

#' Retrieve EGA Username
#'
#' Fetches the EGA username from the system keyring, an environment variable, or
#' an interactive prompt, following that priority order.
#'
#' @param keyring_name Character. Name of the service in the keyring. Defaults
#'   to \code{"REGA_EGA"}.
#' @param envvar Character. Name of the environment variable. Defaults to
#'   \code{"REGA_EGA_USERNAME"}.
#'
#' @return A character string containing the username.
#'
#' @importFrom keyring key_list
#' @importFrom askpass askpass
#'
#' @examples
#' tryCatch(
#'     Rega:::.get_ega_username()
#' )
#'
#' @keywords internal
.get_ega_username <- function(
    keyring_name = "REGA_EGA", envvar = "REGA_EGA_USERNAME"
) {
    .validate_character_scalar(keyring_name)
    .validate_character_scalar(envvar)

    keyring_user = key_list(keyring_name)$username
    envvar_user = Sys.getenv(envvar)

    ega_username <- if (!is.null(keyring_user) && length(keyring_user) > 0) {
        key_list(keyring_name)$username[1]
    } else if (!identical(envvar_user, "")) {
        envvar_user
    } else {
        askpass(prompt = "Please enter your EGA username:")
    }
    ega_username
}

#' Retrieve EGA Password
#'
#' Fetches the EGA password from the system keyring, an environment variable, or
#' an interactive prompt, following that priority order. If using the
#' environment variable, the value is decrypted using a key retrieved via
#' `.get_rega_key`.
#'
#' @param keyring_name Character. Name of the service in the keyring. Defaults
#'   to \code{"REGA_EGA"}.
#' @param envvar Character. Name of the environment variable. Defaults to
#'   \code{"REGA_EGA_PASSWORD"}.
#' @param ... Additional arguments passed to `.get_rega_key`.
#'
#' @return A character string containing the password.
#'
#' @importFrom askpass askpass
#' @importFrom httr2 secret_decrypt
#' @importFrom keyring key_get
#'
#' @examples
#' tryCatch(
#'     ega_password <- Rega:::.get_ega_password()
#' )
#'
#' @keywords internal
.get_ega_password <- function(
    keyring_name = "REGA_EGA", envvar = "REGA_EGA_PASSWORD", ...
) {
    .validate_character_scalar(keyring_name)
    .validate_character_scalar(envvar)

    ega_password <- tryCatch({
        key_get(keyring_name)
    }, error = function(e) {
        rega_key <- .get_rega_key(...)
        ega_password <- Sys.getenv(envvar)

        # Ask for password if not found in environmental variable
        ega_password <- if (identical(ega_password, "")) {
            askpass(prompt = "Please enter your EGA password:")
        } else {
            secret_decrypt(Sys.getenv(envvar), I(rega_key))
        }
    })

    ega_password
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
    token_url = NULL
) {
    if (!is.list(req) && !inherits(req, "httr2_request")) {
        stop("'req' must be an 'httr2_request' or compatible request object.")
    }

    if (is.null(token_url)) {
        token_url <- paste0(
            "https://idp.ega-archive.org/",
            "realms/EGA/protocol/openid-connect/token"
        )
    } else {
        .validate_character_scalar(token_url)
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

    request
}

#' Retrieve EGA API Bearer Token
#'
#' This function retrieves an API token from the European Genome-Phenome Archive
#' (EGA) using user credentials.
#'
#' @param username Character. The username for EGA authentication. Defaults to
#'   the value returned by `.get_ega_username()`.
#' @param password Character. The password for EGA authentication. Defaults to
#'   the value returned by `.get_ega_password()`.
#' @param token_url Character. The URL for the EGA token endpoint. Defaults to
#'   the standard EGA token URL if not provided. Defaults to \code{NULL}.
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
    token_url = NULL
) {
    if (is.null(token_url)) {
        # Use default EGA token URL
        token_url <- paste0(
            "https://idp.ega-archive.org/",
            "realms/EGA/protocol/openid-connect/token"
        )
    } else {
        .validate_character_scalar(token_url)
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

    content
}
