#' ega_parse_body
#' url <- "https://submission.ega-archive.org/api/files"
#' req <- req_method(request(url), "GET")
#' req <- Rega:::ega_oauth(req)
#' req <- req_headers(req, `Content-Type` = "application/json")
#' req <- req_url_query(req, status = NULL, prefix = NULL)
#' resp <- req_perform(req, verbosity = 3)
#' Rega:::ega_parse_body(resp)
#' }
