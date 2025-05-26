## EGA API specification for offline use Source:
## https://submission.ega-archive.org/api/spec/#/ Legal notice:
## https://ega-archive.org/legal-notice/ Re-download the file only if the
## upstream file changes. File 'ega_api_resolved.yaml' was created in the
## following way
## * The EGA API yaml was downloaded from EGA Submitter API website https://submission.ega-archive.org/api/spec/#/
## * References were resolved using Swagger Editor https://editor.swagger.io/.
## * Server URL value on line 12 (`servers/url`) was changed from "/api" to "https://submission.ega-archive.org/api"
## * The schemas for Datasets are not completely correct. There is a possibility to submit a dataset without Runs and Analyses, but this is not reflected in the API. The requirement for `run_provisional_ids` and `analysis_provisional_ids` was commented out (corresponds to lines `13322` and `13323`) in the resolved YAML file to allow for this case.
NULL
