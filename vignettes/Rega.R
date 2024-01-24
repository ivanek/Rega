## ----init, echo=FALSE, results='hide'-----------------------------------------
library(knitr)

## 1. issue with too large vignette, maybe optimizing png?
## unfortunately it requires additional system libs -> not feasible
##           `pngquant=""`, `optipng = "-o7"` both in `opts_chunk`
## SOLUTION: might be to use `fig.retina = 1` and only set it to 2 (default)
##           for figures which requires higher resolution
##           or even decrease res `dpi = 72`, also both in `opts_chunk`
## 
## knit_hooks$set(optipng = hook_optipng,
##                pngquant = hook_pngquant)
##
## 2. crop=NULL or FALSE =>  fix vignette rendering based on yihui/knitr#1796
## added also error=FALSE to include_graphics

opts_chunk$set(
  collapse = TRUE,
  tidy = FALSE,
  fig.retina = 1,
  comment = "#>",
  error = FALSE,
  warning = FALSE,
  message = FALSE,
  crop = NULL                           
)

## check the output type
out_type <- opts_knit$get("rmarkdown.pandoc.to")
if (is.null(out_type))
  out_type <- "html"

## add styling
if (out_type == "html") {
  BiocStyle::markdown()
} else if (out_type == "latex") {
  BiocStyle::latex()
}

## -----------------------------------------------------------------------------
library(tidyverse)
library(Rega)

# set permanent KEY ? 
# 
# `httr2` is built around the notion that the key should live in an environment variable. 
# So the first step is to make your package key available on your local development 
# machine by adding a line to your your user-level `.Renviron` 
# (which you can easily open with `usethis::edit_r_environ())`:

Sys.setenv("REGA_EGA_USERNAME" = "ega-box-2269")
Sys.setenv("REGA_EGA_PASSWORD" = "4f8T_aX-aLP-5obJWEEIShckVjjovgm3")

## -----------------------------------------------------------------------------
ega_users()

## -----------------------------------------------------------------------------
ega_enums()

## -----------------------------------------------------------------------------
ega_enums("study_types")

## -----------------------------------------------------------------------------
ega_enums("run_file_types") 

## -----------------------------------------------------------------------------
ega_enums("genomes") 

## -----------------------------------------------------------------------------
#  ega_submissions()
#  ega_submissions(submission = "EGA00002556014")
#  ega_submissions(submission = "1629667")

## -----------------------------------------------------------------------------
#  ega_studies()
#  ega_studies(study="6743")
#  ega_studies(study="EGAS00001006794")
#  ega_studies(submission="1299997")
#  ega_studies(submission="EGA00002309665")

## -----------------------------------------------------------------------------
#  ega_samples()
#  ega_samples(sample="38812")
#  ega_samples(submission="7290")
#  ega_samples(submission="EGA00002556509")

## -----------------------------------------------------------------------------
#  ega_experiments()
#  ega_experiments(experiment="6971")
#  ega_experiments(submission="7290")
#  ega_experiments(submission="EGA00002556509")

## -----------------------------------------------------------------------------
#  ega_runs()
#  ega_runs(run="42191")
#  ega_runs(submission="7290")
#  ega_runs(submission="EGA00002556509")

## -----------------------------------------------------------------------------
#  ega_analyses()
#  ega_analyses(submission="7290")
#  ega_analyses(submission="EGA00002556509")

## -----------------------------------------------------------------------------
#  ega_datasets()
#  ega_datasets(submission="1300293")
#  ega_datasets(submission="EGA00002309961")

## -----------------------------------------------------------------------------
#  new_submission <- ega_create_submission(title="Test Submission API",
#                                          description="Testing API from R")
#  new_submission

## -----------------------------------------------------------------------------
#  new_submission <- ega_update_submission(submission=new_submission$provisional_id,
#                                          title="Test Submission API (update)",
#                                          description="Testing API from R (update)")
#  new_submission

## -----------------------------------------------------------------------------
#  new_submission <- ega_rollback_submission(submission=new_submission$provisional_id)
#  new_submission

## -----------------------------------------------------------------------------
#  new_submission <- ega_delete_submission(submission=new_submission$provisional_id)
#  new_submission

## -----------------------------------------------------------------------------
#  new_study <- ega_create_study(submission=new_submission$provisional_id,
#                                title="Test Study",
#                                description="New Study, Testing API from R",
#                                study_type="RNASeq")
#  new_study

## -----------------------------------------------------------------------------
#  new_study <- ega_update_study(study=new_study$provisional_id,
#                                     title="Test Study (updated)",
#                                     description="New Study, Testing API from R (updated)",
#                                     study_type="RNASeq")
#  new_study

## -----------------------------------------------------------------------------
#  new_study <- ega_rollback_study(study=new_study$provisional_id)
#  new_study

## -----------------------------------------------------------------------------
#  new_study <- ega_delete_study(study=new_study$provisional_id)
#  new_study

## -----------------------------------------------------------------------------
#  new_sample <- ega_create_sample(submission=new_submission$provisional_id,
#                                  alias="My unique sample alias 1",
#                                  biological_sex="male",
#                                  phenotype="nose",
#                                  subject_id="192873366738836788")
#  
#  new_sample

## -----------------------------------------------------------------------------
#  new_sample <- ega_update_sample(sample=new_sample$provisional_id,
#                                  alias="My unique sample alias 1 (upd)",
#                                  biological_sex="male",
#                                  phenotype="nose",
#                                  subject_id="192873366738836788")
#  new_sample

## -----------------------------------------------------------------------------
#  new_sample <- ega_rollback_sample(sample=new_sample$provisional_id)
#  new_sample

## -----------------------------------------------------------------------------
#  new_sample <- ega_delete_sample(sample=sample$provisional_id)
#  new_sample

## -----------------------------------------------------------------------------
#  new_experiment <-  ega_create_experiment(submission=new_submission$provisional_id,
#                                           design_description = "My experiment design",
#                                           study_provisional_id = new_study$provisional_id,
#                                           instrument_model_id = 1,
#                                           library_layout ="SINGLE",
#                                           library_strategy = "WGS",
#                                           library_source = "GENOMIC",
#                                           library_selection ="RANDOM")
#  
#  new_experiment

## -----------------------------------------------------------------------------
#  new_experiment <- ega_update_experiment(experiment=new_experiment$provisional_id,
#                                          design_description = "My experiment design (updated)",
#                                          study_provisional_id = new_study$provisional_id,
#                                          instrument_model_id = 1,
#                                          library_layout ="SINGLE",
#                                          library_strategy = "WGS",
#                                          library_source = "GENOMIC",
#                                          library_selection ="RANDOM")
#  new_experiment

## -----------------------------------------------------------------------------
#  new_experiment <- ega_rollback_experiment(experiment=new_experiment$provisional_id)
#  new_experiment

## -----------------------------------------------------------------------------
#  new_experiment <- ega_delete_experiment(experiment=new_experiment$provisional_id)
#  new_experiment

## -----------------------------------------------------------------------------
#  new_run <- ega_create_run(submission=new_submission$provisional_id,
#                            run_file_type="fastq",
#                            files=list("75243", "75406"),
#                            experiment_provisional_id=new_experiment$provisional_id,
#                            sample_provisional_id=new_sample$provisional_id)
#  new_run

## -----------------------------------------------------------------------------
#  new_run <- ega_update_run(run=new_run$provisional_id,
#                            run_file_type="fastq",
#                            files=list("75243", "75406"),
#                            experiment_provisional_id=new_experiment$provisional_id,
#                            sample_provisional_id=new_sample$provisional_id)
#  new_run

## -----------------------------------------------------------------------------
#  new_run <- ega_rollback_run(run=new_run$provisional_id)
#  new_run

## -----------------------------------------------------------------------------
#  new_run <- ega_delete_run(run=new_run$provisional_id)
#  new_run

## -----------------------------------------------------------------------------
#  new_analysis <- ega_create_analysis(submission=new_submission$provisional_id,
#                                      title="My first analysis",
#                                      description = "Description of my first analysis",
#                                      analysis_type="REFERENCE ALIGNMENT",
#                                      files=list("188544"),
#                                      study_provisional_id = new_study$provisional_id,
#                                      experiment_provisional_ids = list(new_experiment$provisional_id),
#                                      sample_provisional_ids = list(new_sample$provisional_id),
#                                      experiment_types = list("Whole genome sequencing"),
#                                      genome_id = 27)
#  new_analysis

## -----------------------------------------------------------------------------
#  new_analysis <- ega_update_analysis(analysis=new_analysis$provisional_id,
#                            title="My first analysis (update)",
#                                      description = "Description of my first analysis",
#                                      analysis_type="REFERENCE ALIGNMENT",
#                                      files=list("188544"),
#                                      study_provisional_id = new_study$provisional_id,
#                                      experiment_provisional_ids = list(new_experiment$provisional_id),
#                                      sample_provisional_ids = list(new_sample$provisional_id),
#                                      experiment_types = list("Whole genome sequencing"),
#                                      genome_id = 27)
#  new_analysis

## -----------------------------------------------------------------------------
#  new_analysis <- ega_rollback_analysis(analysis=new_analysis$provisional_id)
#  new_analysis

## -----------------------------------------------------------------------------
#  new_analysis <- ega_delete_analysis(analysis=new_analysis$provisional_id)
#  new_analysis

## -----------------------------------------------------------------------------
#  new_dataset <- ega_create_dataset(submission = new_submission$provisional_id,
#                                    title = "My dataset title",
#                                    description = "My dataset description",
#                                    dataset_types = list("Transcriptome profiling by high-throughput sequencing"),
#                                    policy_accession_id="EGAP00001003010",
#                                    run_provisional_ids=list(new_run$provisional_id),
#                                    analysis_provisional_ids = list(new_analysis$provisional_id))
#  
#  new_dataset

## -----------------------------------------------------------------------------
#  new_dataset <- ega_update_dataset(dataset = new_dataset$provisional_id,
#                                    title = "My dataset title (update)",
#                                    description = "My dataset description",
#                                    dataset_types = list("Transcriptome profiling by high-throughput sequencing"),
#                                    policy_accession_id="EGAP00001003010",
#                                    run_provisional_ids=list(new_run$provisional_id),
#                                    analysis_provisional_ids = list(new_analysis$provisional_id))
#  new_dataset

## -----------------------------------------------------------------------------
#  new_dataset <- ega_rollback_dataset(dataset=new_dataset$provisional_id)
#  new_dataset

## -----------------------------------------------------------------------------
#  new_dataset <- ega_delete_dataset(dataset=new_dataset$provisional_id)
#  new_dataset

## -----------------------------------------------------------------------------
#  
#  # ega_finalise_submission(submission = new_submission$provisional_id,
#  #                         expected_release_date="2026-01-01")

## -----------------------------------------------------------------------------
#  # check the authentication flow
#  
#  # # token
#  # token <- oauth_flow_password(
#  #   client = ega_client,
#  #   username = ega_username,
#  #   password = ega_password
#  # )
#  # token$access_token
#  
#  # # cached token
#  # token <- oauth_token_cached(
#  #   client = ega_client,
#  #   flow = oauth_flow_password,
#  #   flow_params = list(
#  #     username = ega_username,
#  #     password = ega_password
#  #   ),
#  #   cache_disk = TRUE
#  # )
#  # token$access_token

