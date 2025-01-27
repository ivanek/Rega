#' Convert API Names to Prettified Labels
#'
#' This function converts API-style names with underscores into human-readable
#' labels by replacing underscores with spaces and applying title case.
#'
#' @param x Character vector. API field names to be converted.
#'
#' @return A character vector with API names converted to human-readable labels.
#'
#' @importFrom tools toTitleCase
#'
#' @examples
#' api_name_to_label(c("first_name", "last_name", "instument_model"))
#'
#' @export
api_name_to_label <- function(x) {
  toTitleCase(gsub("_", " ", x))
}

#' Convert Prettified Labels to API Names
#'
#' @param x Character vector. Prettified labels to convert.
#' @param req_str Character. Optional prefix to remove from labels. Defaults to
#' `"* "`.
#'
#' @return A character vector with labels converted to API-style names.
#'
#' @examples
#' label_to_api_name(c("* First Name", "Last Name"))
#' label_to_api_name(c("# Instrument Model", "# Fragment SD"), req_str = "# ")
#'
#' @export
label_to_api_name <- function(x, req_str = "* ") {
  req_str <- paste0("^\\", req_str)
  tolower(gsub(" ", "_", gsub(req_str, "", x)))
}

#' Use First Row as Column Names for a Data Frame
#'
#' @param df Data frame. The input data frame whose first row will become column
#' names.
#' @param to_api Logical. Whether to convert labels to API-style names using
#' `label_to_api_name()`. Defaults to `TRUE`.
#'
#' @return A data frame with updated column names and the first row removed.
#'
#' @examples
#' df = data.frame(id = c("A B", "C D_"), value = c("* E F", "GH"))
#'
#' first_row_to_colnames(df)
#' first_row_to_colnames(df, to_api = FALSE)
#'
#' @export
first_row_to_colnames <- function(df, to_api = TRUE) {
  first_row <- as.character(df[1, ])
  if (to_api) {
    first_row <- label_to_api_name(first_row)
  }
  # Set first row as names
  names(df) <- make.names(first_row, unique = TRUE)
  # Remove first row
  df <- df[-1, ]
  return(df)
}

#' Add a Column to a Data Frame Based on Lookup Table
#'
#' This function adds a new column to a data frame by mapping values from an
#' existing column through a lookup table.
#'
#' @param df A data frame to which the new column will be added.
#' @param to A string specifying the name of the new column.
#' @param from A string specifying the name of the column to map values from.
#' @param lut A named list or vector serving as the lookup table.
#'
#' @return The input data frame with the added column.
#'
#' @examples
#' df <- data.frame(id = c("A", "B", "C"), stringsAsFactors = FALSE)
#' lut <- list(A = 1, B = 2, C = 3)
#' lut_add(df, "value", "id", lut)
#'
#' @export
lut_add <- function(df, to, from, lut) {
  new_col <- lapply(df[[from]], function(x) {
    unname(lut[x])
  })
  # Unlist to if all rows of original column are of length 1
  if (all(vapply(df[[from]], \(x) length(x) == 1, FUN.VALUE = logical(1)))) {
    new_col <- unlist(new_col)
  }
  df[[to]] <- new_col
  return(df)
}

#' Add Multiple Lookup-Based Columns to a Data Frame
#'
#' Adds multiple columns to a data frame by applying multiple lookup tables,
#' each defined by a set of arguments specifying the new column, the source
#' column, and the lookup table.
#'
#' @param df A data frame to which new columns will be added.
#' @param ... A series of lists, each containing three elements: the name of
#'   the new column (\code{to}), the name of the source column (\code{from}),
#'   and the lookup table (\code{lut}). See \code{\link{lut_add}} for details.
#'
#' @return The input data frame with the added columns.
#'
#' @importFrom rlang enquos eval_tidy
#'
#' @examples
#' df <- data.frame(id = c("A", "B", "C"), stringsAsFactors = FALSE)
#' lut1 <- list(A = 1, B = 2, C = 3)
#' lut2 <- list(A = "x", B = "y", C = "z")
#' multi_lut_add(df, list("value1", "id", lut1), list("value2", "id", lut2))
#'
#' @export
multi_lut_add <- function(df, ...) {
  args <- enquos(...)
  dots <- lapply(args, eval_tidy)

  stop_msg <- ("Objects need to be list of length 3, containing
              column names 'to', column name 'from' and a lookup table.
              See 'lut_add' for details.")

  lapply(dots, function(x) {
    if (length(x) != 3 ||
      !is.character(x[[1]]) ||
      !is.character(x[[2]])) {
      stop(stop_msg)
    }
    if (is.null(names(x[[3]]))) {
      stop(stop_msg)
    }
  })

  result_df <- Reduce(
    function(a, x) {
      lut_add(a, x[[1]], x[[2]], x[[3]])
    },
    dots,
    init = df
  )

  return(result_df)
}

#' Format Aliases from a Table
#'
#' @param tab Data frame. The input table where the first row contains column
#' names.
#' @param params List. Additional parameters for formatting. Takes a formatter
#' params value from parser parameter yaml file. Currently unused.
#'
#' @return A named list where each name corresponds to a formatted column name,
#' and values are non-NA elements of the respective column.
#'
#' @examples
#' tab <- data.frame(Alias = c("name1", "name2", NA), Value = c(1, 2, 3))
#' aliases_formatter(tab, params = list())
#'
#' @export
aliases_formatter <- function(tab, params) {
  tab <- first_row_to_colnames(tab)
  l <- lapply(as.list(tab), \(x) x[!is.na(x)])
  names(l) <- label_to_api_name(names(l))
  return(l)
}

#' Format a Column Table
#'
#' @param tab Data frame. The input table sumbission metadata file where the
#' first row contains column names.
#' @param params List. Additional parameters for formatting. Takes a formatter
#' params value from parser parameter yaml file. Currently unused.
#'
#' @return A cleaned data frame with column names set from the first row, empty
#' rows removed, and whitespace trimmed from all values.
#'
#' @importFrom stringr str_trim
#'
#' @examples
#' df = data.frame(
#'   ...1 = c("* Alias", "Sample1", "Sample2"),
#'   ...2 = c("* Phenotype", "wt", "ko"),
#'   ...3 = c("Description", NA, NA)
#' )
#'
#' column_table_formatter(df, list())
#'
#' @export
column_table_formatter <- function(tab, params) {
  tab <- first_row_to_colnames(tab)
  # Remove completely empty rows
  tab <- tab[!apply(is.na(tab), 1, all), ]
  # Remove whitespaces
  tab[] <- lapply(tab, \(x) str_trim(as.character(x)))

  if (is.list(params) && length(params$fold > 0)) {
    tab <- Reduce(
      function(m, p) fold_column(m, p, p),
      params$fold,
      init = tab
    )
  }

  return(tab)
}

#' Format a Row Table
#'
#' @param tab Data frame. The input table from a sumbission metadata file
#' @param params List. Additional parameters for formatting. Takes a formatter
#'   params value from parser parameter yaml file.
#'
#' @return A cleaned and formatted tibble with correctly organized rows and
#'   columns, whitespace trimmed, and folding applied to specified columns.
#'
#' @importFrom stringr str_trim
#' @importFrom utils head
#' @importFrom tibble as_tibble
#'
#' @examples
#' # Formatter parameters
#' params = list(fold = "extra_attributes")
#'
#' # Sample data frame
#' df = data.frame(
#'   ...1 = c("* Study", "* Title", "Extra Attributes", "Extra Attributes"),
#'   ...2 = c("Study1", "Title1", "A", "B"),
#'   ...3 = c("* Study", "* Title", "Extra Attributes", NA),
#'   ...4 = c("Study2", "Title2", "C", NA)
#' )
#'
#' row_table_formatter(df, params)
#'
#' @export
row_table_formatter <- function(tab, params) {
  tab <- tab[, colSums(!is.na(tab)) > 0]
  if (!(ncol(tab) %% 2 == 0)) {
    stop("Incorrect column organization of the table columns.")
  }
  split_tab <- lapply(seq(1, ncol(tab), by = 2), function(i) {
    cols <- i:min(i + 1, ncol(tab))
    st <- tab[, cols, drop = FALSE]
    st[[1]] <- label_to_api_name(st[[1]])
    # Remove completely empty rows that are created when the different entries
    # in the template don't match row-wise
    st <- st[!apply(is.na(st), 1, all), ]
    st <- as.data.frame(t(st))
    st <- first_row_to_colnames(st, to_api = FALSE)
    rownames(st) <- NULL
    # Remove whitespaces before folding
    st[] <- lapply(st, \(x) str_trim(as.character(x)))
    # Fold the values with multiple possible entries
    # Use for loop to change the values in place
    for (f in params$fold) st <- fold_column(st, f, f)
    st
  })

  # Impose column ordering of the first entry (at least one is required)
  column_order <- names(split_tab[[1]])

  tab <- do.call(
    rbind,
    lapply(split_tab, function(x) {
      x[, match(names(x), column_order)]
    })
  )

  return(as_tibble(tab))
}


# Keep in case multi table will be needed later
multi_table_formatter <- function(tab, params) {
  anchor_number <- params$anchor_number
  # Remove empty rows
  tab <- tab[rowSums(is.na(tab)) != ncol(tab), ]

  anchor_names <- tab[seq_len(anchor_number), 1][[1]]
  tab_starts <- which(tab[[1]] == anchor_names[1])
  if (length(tab_starts) > 1) {
    tab_ends <- c(tab_starts[2:length(tab_starts)] - 1, nrow(tab))
  } else {
    tab_ends <- nrow(tab)
  }

  # Discard empty tables
  # +1 for header row (not part of table)
  has_samples <- (tab_ends - tab_starts) > (anchor_number + 1)
  tab_starts <- tab_starts[has_samples]
  tab_ends <- tab_ends[has_samples]

  # If no data in the sheet
  if (length(tab_starts) == 0 || length(tab_ends) == 0) {
    tab <- tab[-1, ]
    tab <- first_row_to_colnames(tab)
    names(tab) <- label_to_api_name(names(tab))
    tab <- tab[0, ]
  } else {
    tab <- Map(function(i, j) {
      # Select the anchor values for the table
      anchor_values <- tab[i:(i + anchor_number - 1), 2][[1]]
      # Filter rows that contain data for single table
      tab <- tab[(i + anchor_number):j, ]
      tab <- first_row_to_colnames(tab)
      # Add the anchor data to table for linking
      tab[anchor_names] <- as.list(anchor_values)
      names(tab) <- label_to_api_name(names(tab))
      tab
    }, tab_starts, tab_ends)
    tab <- do.call(rbind, tab)
  }
  return(tab)
}

#' Format File Table with EGA File Paths
#'
#' @param tab Data frame. The input table containing file information. Columns
#' `file`, `ega_inbox_relative_path` need to be present in the data.
#' @param params List. Additional parameters for formatting. Takes a formatter
#' params value from parser parameter yaml file. Includes `crypt_ext` for
#' encryption file extensions and `prepend_slash` to control path prefix.
#'
#' @return A formatted data frame with cleaned column names, and updated
#' `ega_file` paths based on file and relative path information.
#'
#' @importFrom stringr str_trim
#'
#' @examples
#' params = list(prefix = "", crypt_ext = "c4gh", prepend_slash = FALSE)
#'
#' # Dummy data, first row will be moved to column names
#' tab = data.frame(
#'   x1 = c("file", "value1", "value2"),
#'   x2 = c("ega_inbox_relative_path", NA, "proj1")
#' )
#'
#' file_formatter(tab, params)
#'
#' @export
file_formatter <- function(tab, params) {
  tab <- first_row_to_colnames(tab)
  names(tab) <- label_to_api_name(names(tab))
  # Strip white space
  tab[] <- lapply(tab, \(x) str_trim(as.character(x)))
  # Keep the original names and modify the ega paths
  tab$ega_file <- tab$file

  if (is.character(params$crypt_ext) && nchar(params$crypt_ext) > 0) {
    tab$ega_file <- paste(tab$ega_file, params$crypt, sep = ".")
  }

  if (is.character(params$prefix) && nchar(params$prefix) > 0) {
    tab$ega_file <- paste0(params$prefix, tab$ega_file)
  }

  for (i in seq_len(nrow(tab))) {
    ef <- tab$ega_file[i]
    ep <- tab$ega_inbox_relative_path[i]

    # Strip the initial slash if it exists
    ep <- sub("\\^\\/", "", ep)

    # Construct the new path if ega relative path is specified
    if (!is.na(ep) && is.character(ep) && nchar(ep) > 0) {
      ef <- file.path(ep, ef)
    }

    # Prepend a slash if specified
    if (params$prepend_slash) {
      ef <- file.path("", ef)
    }

    tab$ega_file[i] <- ef
  }

  return(tab)
}

#' Retrieve a Formatter Function by Type of Submission Metadata Table
#'
#' @param x Character. The name of the submission metadata table/sheet.
#' @param params List. A list containing a `formatter` element from parser
#' parameter yaml file.
#'
#' @return The formatter function corresponding to the specified table.
#'
#' @examples
#' # Load formatter params
#' params <- yaml::read_yaml(system.file(
#'   "extdata/default_parser_params.yaml", package = "Rega"
#' ))
#'
#' # Dummy data, first row will be moved to column names
#' tab = data.frame(
#'   x1 = c("file", "value1", "value2"),
#'   x2 = c("ega_inbox_relative_path", NA, "proj1")
#' )
#'
#' ff = get_formatter("files", params)
#' ff_params = get_formatter_params("files", params)
#'
#' ff(tab, ff_params)
#'
#' @export
get_formatter <- function(x, params) {
  return(get(params$formatter[[x]][["type"]]))
}

#' Retrieve Formatter Parameters by Name
#'
#' @param x Character. The name of the formatter for which to retrieve
#' parameters.
#' @param params List. A list containing a `formatter` element from parser
#' parameter yaml file.
#'
#' @return A list of parameters for the specified formatter.
#'
#' @examples
#' # Load formatter params
#' params <- yaml::read_yaml(system.file(
#'   "extdata/default_parser_params.yaml", package = "Rega"
#' ))
#'
#' # Dummy data, first row will be moved to column names
#' tab = data.frame(
#'   x1 = c("file", "value1", "value2"),
#'   x2 = c("ega_inbox_relative_path", NA, "proj1")
#' )
#'
#' ff = get_formatter("files", params)
#' ff_params = get_formatter_params("files", params)
#'
#' ff(tab, ff_params)
#'
#' @export
get_formatter_params <- function(x, params) {
  return(params$formatter[[x]][["params"]])
}

#' Fold Columns with a Common Prefix into a Single Column Nested as List
#'
#' If `NA` values are present in any of the columns to be nested, they
#' will be removed. If the column is not present it will be added with
#' `NA` as a single value.
#'
#' @param tab Data frame. The input table with columns to fold.
#' @param column_prefix Character. The prefix of columns to nest into a single
#' column represented as list.
#' @param new_name Character. The name of the new folded column.
#'
#' @return A data frame with the specified columns nested into a single column.
#'
#' @importFrom stats na.omit
#'
#' @examples
#' tab <- data.frame(id = c(1,2), name.1 = c("A1", NA), name.2 = c("B1", "B2"))
#' fold_column(tab, "name", "folded_column")
#'
#' @export
fold_column <- function(tab, column_prefix, new_name) {
  tmp_fold_name <- NULL # nolint
  # TODO fix examples

  selected_columns <- tab[grepl(paste0("^", column_prefix), names(tab))]
  iter_columns <- unname(as.list(as.data.frame(t(selected_columns))))

  folded_columns <- lapply(
    iter_columns,
    function(row) {
      ft <- as.character(na.omit(row))
      # if the original column prefix didn't contain any values, it will replace
      # the resulting character(0) with NA for easier handling also if the
      # column didn't exist it will add it with NA value
      if (is.null(ft) || (is.character(ft) && length(ft) == 0)) {
        ft <- NA_character_
      }
      ft
    }
  )

  tab$tmp_fold_name <- folded_columns

  # Drop columns starting with column_prefix
  tab <- tab[, !grepl(paste0("^", column_prefix), names(tab))]
  colnames(tab)[colnames(tab) == "tmp_fold_name"] <- new_name

  return(tab)
}

#' Link Data Between Metadata Sheets
#'
#' Data frames representing metadata sheets that contain column names
#' corresponding to `sheet_name` containing an ID reference (to a first column
#' in `sheet_name`) will be replaced with the rest of the values nested as a
#' list.
#'
#' @param metadata List. A list of data frames representing metadata sheets.
#' @param sheet_name Character. The name of the sheet to link with other sheets.
#'
#' @return A list of updated metadata with the specified values replaced based
#'   on referenced values present in `sheet_name`.
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Link data from a specific sheet to other sheets in metadata
#' metadata <- list(
#'   sheet1 = data.frame(id = c(1, 2), linked_sheet = c("A", "B")),
#'   linked_sheet = data.frame(id = c("A", "B"), value = c(10, 20))
#' )
#' updated_metadata <- link_sheet(metadata, "linked_sheet")
#' }
#'
#' @export
link_sheet <- function(metadata, sheet_name) {
  # TODO check examples
  has_linked_sheets <- vapply(metadata, function(x) {
    # check if the sheet to be linked is present and if there is at least
    # one non-NA value
    sheet_name %in% names(x) && !(all(is.na(x[[sheet_name]])))
  }, FUN.VALUE = logical(1))

  if (nrow(metadata[[sheet_name]]) == 0) {
    if (any(has_linked_sheets)) {
      err_msg <- sprintf(
        "Linked data %s in sheets %s was specified, but no entries were found.",
        sheet_name, paste0(names(metadata)[has_linked_sheets], collapse = ",")
      )
      stop(err_msg)
    }
  }

  # Use for loop to change values in place
  for (x in names(metadata)[has_linked_sheets]) {
    # Data in the template are matched based on the value of the first column
    merge_y <- names(metadata[[sheet_name]])[1]
    metadata[[x]][[sheet_name]] <- lapply(
      metadata[[x]][[sheet_name]], function(y) {
        if (all(is.na(y))) {
          list()
        } else {
          # Merge with the linked sheet, keep as a data frame for proper JSON
          # parsing into list of 'dictionaries'.
          mdf <- merge(
            setNames(data.frame(y), merge_y),
            metadata[[sheet_name]],
            by = merge_y
          )

          # API specific processing
          if (sheet_name == "collaborators") mdf$id <- as.integer(mdf$id)
          mdf
        }
      }
    )
  }
  return(metadata)
}

#' Process Delimited Columns in Metadata
#'
#' The specified column name is searched for across all the data frames. If the
#' column is `pubmed_ids`, values are converted to integers.
#'
#' @param metadata List. A list of data frames representing metadata sheets.
#' @param column_name Character. The name of the column to process.
#' @param separator Character. The delimiter used to split column values.
#'
#' @return A list of updated metadata with the specified column split into lists
#'   based on the delimiter and trimmed.
#'
#' @importFrom stringr str_split str_trim
#'
#' @examples
#' metadata <- list(
#'   sheet1 = data.frame(pubmed_ids = c("123; 456", "130; 789; 102", NA))
#' )
#'
#' process_delimited_column(metadata, "pubmed_ids", ";")
#'
#' @export
process_delimited_column <- function(metadata, column_name, separator) {
  # use for loop to change values in place
  for (sheet in names(metadata)) {
    if (column_name %in% names(metadata[[sheet]])) {
      vals <- str_split(metadata[[sheet]][[column_name]], separator)
      vals <- lapply(vals, str_trim)
      # Complicated lapply/ifelse due to the format that needs to be returned
      # in order to be correctly parsed to JSON
      metadata[[sheet]][[column_name]] <- lapply(vals, function(y) {
        if (all(is.na(y))) {
          list()
        } else {
          # API specific processing
          # Pubmed ids need to be integer, ifelse doesn't work on vectors
          if (column_name == "pubmed_ids") {
            as.integer(y)
          } else {
            y
          }
        }
      })
    }
  }

  return(metadata)
}

#' Retrieve Chromosome Belonging to a Group
#'
#' @param group_id Character. The group ID to filter by.
#' @param chr_enum Character vector. Chromosome enumeration data, where each
#'   element is a string containing fields separated by `sep`. Enum data is
#'   created by `parse_enum` and `get_enum` functions. See vignette for more
#'   details.
#' @param sep Character. Field separator in a string. Defaults to `"--"`
#'
#' @return An integer vector of chromosome IDs corresponding to the specified
#'   group ID.
#'
#' @importFrom stats setNames
#' @importFrom stringr str_split
#'
#' @examples
#' get_chr_group(
#'  "group1", c("group1--1--chr1--name1", "group2--2--chr2--name2")
#' )
#'
#' @export
get_chr_group <- function(group_id, chr_enum, sep = "--") {
  chr_enum_split <- str_split(chr_enum, sep)
  df <- setNames(
    data.frame(t(data.frame(chr_enum_split))),
    c("group_id", "id", "label", "name")
  )
  rownames(df) <- NULL

  df <- df[df$group_id == group_id, c("id", "label")]
  df[["id"]] <- as.integer(df[["id"]])
  return(df)
}

#' Format Chromosome Metadata
#'
#' Formats and processes chromosome-related metadata from an input object by
#' applying chromosome group lookups or splitting chromosome strings from the
#' EGA enums.
#'
#' @param metadata List. A list of data frames representing metadata sheets,
#'   containing \code{analyses}. Each row in \code{analyses} can have entry in
#'   \code{chromosomes} or \code{chromosome_groups} column.
#'
#' @return A list of formatted chromosome data extracted or computed from the
#'   input metadata.
#'
#' @importFrom stringr str_split_i
#'
#' @examples
#' # Mock metadata data frame
#' metadata <- list(
#'   analyses = data.frame(
#'     chromosomes = I(list(
#'       NA,
#'       list("group1--1--chr1--name1", "group2--3--chr3--name3"))
#'     ),
#'     chromosome_groups = c("group1", NA),
#'     stringsAsFactors = FALSE
#'   ),
#'   select_input_data = list(
#'     chromosomes = c("group1--1--chr1--name1", "group1--2--chr2--name2"))
#' )
#'
#' format_chromosomes(metadata)
#'
#' @export
format_chromosomes <- function(metadata) {
  # TODO fix examples
  chr_list <- apply(metadata$analyses, 1, function(x) {
    if (all(is.na(x$chromosomes))) {
      if ("chromosome_groups" %in% names(x) && !is.na(x$chromosome_groups)) {
        get_chr_group(
          x$chromosome_groups,
          metadata$select_input_data$chromosomes
        )
      } else {
        return(list())
      }
    } else {
      data.frame(
        id = as.integer(unique(str_split_i(x$chromosomes, "--", 2))),
        label = unique(str_split_i(x$chromosomes, "--", 3))
      )
    }
  })
  return(chr_list)
}
