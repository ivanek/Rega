#' List All Samples in EGA
#'
#' Get information about existing samples in EGA. If provisional or permanent accession `sample` id is specified, then only information about this sample is shown.
#'
#' @param sample Get information only for this sample.
#' @param submission Get information for all samples in this submission.
#'
#' @return tibble with information about samples.
#' @export
#'
#' @examples \dontrun{
#' ega_samples()
#' }
#'
ega_samples <- function(submission=NULL, sample=NULL) {
  # information about single sample
  # sample has preference over submission
  if (!is.null(sample)) {
    ret <- ega_get(resource_prefix="samples",
                   resource_id = sample)
  } else if (!is.null(submission)) {
    # information about all studies in submission
    ret <- ega_get(resource_prefix="submissions",
                   resource_id = submission,
                   resource_suffix = "samples")
  } else {
    ret <- ega_get(resource_prefix="samples")
  }
  return(ret)
}



#' Create Sample In EGA
#'
#' Create a single sample in EGA.
#'
#' @param submission character scalar. Provisional or stable submission id.
#' @param alias character scalar. Sample alias (must be unique within the study).
#' @param biological_sex character scalar. Biological sex of subject, for accepted values check `ega_enums("biological_sex")`.
#' @param subject_id character scalar. Subject Id.
#' @param phenotype character scalar. Phenotype group
#' @param title character scalar. Sample title.
#' @param description character scalar. Sample description
#' @param biosample_id character scalar. Biosample Id.
#' @param case_control character scalar. Case or Control, for accepted values check `ega_enums("case_controls")`.
#' @param organism_part character scalar. Sample tissue or cell type.
#' @param cell_line character scalar. Name of teh cell line
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return `tibble` with information about the created sample.
#' @export
#' @describeIn ega_create_sample Create a single sample.
#'
#' @examples \dontrun{
#' new_sample <- ega_create_sample(submission=provisional_id,
#' alias="My unique sample alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_create_sample <- function(submission, alias, biological_sex,
                              subject_id, phenotype,
                              title=NULL, description=NULL,
                              biosample_id=NULL, case_control=NULL,
                              organism_part=NULL, cell_line=NULL,
                              extra_attributes=NULL) {

  extra_attributes <- check_list(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("submissions/",submission,"/samples"),
          method="POST",
          alias=alias, biological_sex=biological_sex,
          subject_id=subject_id, phenotype=phenotype,
          title=title, description=description,
          biosample_id=biosample_id, biosample_id=biosample_id,
          organism_part=organism_part, cell_line=cell_line,
          extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Update sample In EGA
#'
#' Update information of this sample.
#'
#' @param sample character scalar. Sample Id.
#' @param alias character scalar. Sample alias (must be unique within the study).
#' @param biological_sex character scalar. Biological sex of subject, for accepted values check `ega_enums("biological_sex")`.
#' @param subject_id character scalar. Subject Id.
#' @param phenotype character scalar. Phenotype group
#' @param title character scalar. Sample title.
#' @param description character scalar. Sample description
#' @param biosample_id character scalar. Biosample Id.
#' @param case_control character scalar. Case or Control, for accepted values check `ega_enums("case_controls")`.
#' @param organism_part character scalar. Sample tissue or cell type.
#' @param cell_line character scalar. Name of teh cell line
#' @param extra_attributes list. The extra attributes, list with element names: `tag`, `value` and `unit.`
#'
#' @return tibble. Information about updated sample.
#' @export
#'
#' @examples \dontrun{
#' ega_update_sample(sample=1,
#' alias="My unique sample alias 1",
#' biological_sex="male",
#' phenotype="nose",
#' subject_id="192873366738836788")
#' }
ega_update_sample <- function(sample, alias, biological_sex,
                             subject_id, phenotype,
                             title=NULL, description=NULL,
                             biosample_id=NULL, case_control=NULL,
                             organism_part=NULL, cell_line=NULL,
                             extra_attributes=NULL) {

  extra_attributes <- check_list(extra_attributes,
                                 nms=c("tag", "value", "unit"))
  resp <- req_ega(paste0("samples/", sample),
                  method="PUT",
                  alias=alias, biological_sex=biological_sex,
                  subject_id=subject_id, phenotype=phenotype,
                  title=title, description=description,
                  biosample_id=biosample_id, biosample_id=biosample_id,
                  organism_part=organism_part, cell_line=cell_line,
                  extra_attributes=extra_attributes)
  resp <- ega_parse_body(resp)
  return(resp)
}

#' Delete Sample In EGA
#'
#' Delete this sample.
#'
#' @param sample character scalar. Provisional sample Id.
#'
#' @return tibble. Information about deleted sample.
#' @export
#'
#' @examples \dontrun{
#' ega_delete_sample(sample=1)
#' }
ega_delete_sample <- function(sample) {
  resp <- req_ega(paste0("samples/", sample),
                  method="DELETE")
  resp <- ega_parse_body(resp)
  return(resp)
}


#' Rollback Sample In EGA
#'
#' Undo all the changes done to this sample.
#'
#' @param sample character scalar. Sample accession Id.
#'
#' @return tibble. Information about rollback of the sample.
#' @export
#'
#' @examples \dontrun{
#' ega_rollback_sample(sample=1)
#' }
ega_rollback_sample <- function(sample) {
  resp <- req_ega(paste0("samples/", sample, "/rollback"),
                  method="PUT")
  resp <- ega_parse_body(resp)
  return(resp)
}
