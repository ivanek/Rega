#' Check Properties Of The List
#'
#' Helper function to verify names and structure of a list.
#' All elements have to have the same length.
#'
#' @param x list to be checked.
#' @param names Optional. Character vector with element names.
#' @param types Optional. Character vector with types of elements, recycled if needed.
#' @param null.ok `NULL` is ok
#'
#' @return Logical. TRUE if conditions are full-filled.
#' @keywords internal
#' @noRd
#'
#' @importFrom checkmate check_list
#' @importFrom checkmate check_names
#' @importFrom checkmate check_numeric
#' @importFrom checkmate check_integer
#' @importFrom checkmate check_character
#' @importFrom checkmate check_choice
#' @importFrom checkmate makeAssertionFunction
#'
#' @examples
#' extra <- list(a="x", b=2)
#' Rega:::check_ega_list(ll=extra, nms=c("a","b"), type=c("character", "numeric"))
#'
check_ega_list <- function(x, names=NULL, types=NULL, null.ok=TRUE) {
  res <- checkmate::check_list(x, null.ok=null.ok)
  if (!isTRUE(res))
    return(res)
  if (length(unique(length(x))) > 1) {
    return("All list elements must have same length!")
  }
  if (!is.null(names)) {
    if (!checkmate::check_names(names(x), permutation.of=names)) {
      return("Names of the elements do not match expected ones!")
    } else {
      x <- x[names]
    }
  }
  if (!is.null(x) && !is.null(types)) {
    if (length(types) < length(x)) {
      types <- rep(types, length.out=length(x))
    }
    for (i in seq_along(x)) {
      tp <- checkmate::check_choice(types[i], c("character", "numeric", "integer"))
      if (!isTRUE(tp))
        return(tp)
      f <- eval(parse(text=paste0("checkmate::check_", tp)))
      res <- f(x[[i]])
      if (!isTRUE(res))
        return(res)
    }
  }
  return(TRUE)
}

#TODO:  how to document this?
# print(assert_ega_list)
assert_ega_list <- checkmate::makeAssertionFunction(check_ega_list)

#' Assert Provisional And Accession Id
#'
#' Helper function to make sure the provided Id is matching either provisional
#' (integerish) Id or permanent EGA accession Id with defined pattern.
#'
#' @param x Character or integerish scalar. Id to be checked.
#' @param pattern Optional pattern of permanent accession to be fullfilled.
#'
#' @importFrom checkmate check_integerish
#' @importFrom checkmate check_string
#' @importFrom checkmate assert
#'
#' @return Character or integerish scalar. Checked Id.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' id <- assert_ega_id(12345)
#' id <- assert_ega_id("EGA002", pattern="^EGA\\d{3}$")
assert_ega_id <- function(x, pattern=NULL) {
  checkmate::assert(
    checkmate::check_integerish(x, len=1, null.ok = TRUE),
    checkmate::check_string(x, pattern=pattern, null.ok = TRUE),
    combine = "or")
}
