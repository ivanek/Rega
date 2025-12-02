#!/bin/bash
## EGA API specification for offline use.
## Source:
## https://submission.ega-archive.org/api/spec/#/ Legal notice:
## https://ega-archive.org/legal-notice/ Re-download the file only if the
## upstream file changes. File 'ega_api_deref.yaml' was created in the
## following way
## * The EGA API yaml was downloaded from EGA Submitter API website https://submission.ega-archive.org/api/spec/#/
## * References were resolved using redocly CLI https://redocly.com/docs/cli.
## * Server URL value on line 10 (`servers/url`) was changed from "/api" to "https://submission.ega-archive.org/api"
## * The schemas for Datasets are not completely correct. There is a possibility to submit a dataset without Runs and Analyses, but this is not reflected in the API. The requirement for `run_provisional_ids` and `analysis_provisional_ids` was commented out (corresponds to lines `4824` and `4825`) in the resolved YAML file to allow for this case.

INPUT_FILE="ega_api.yaml"
FILE="inst/extdata/ega_api_deref.yaml"
NEW_URL="https://submission.ega-archive.org/api"
ESCAPED_URL=${NEW_URL//&/\\&}
URL_LINE=10
COMMENT_LINE1=4824
COMMENT_LINE2=4825

npx @redocly/cli@2.1.5 bundle "$INPUT_FILE" -o "$FILE" --dereferenced

if sed --version >/dev/null 2>&1; then
  sed -i -e "${URL_LINE}s|^\([[:space:]]*\)- url:[[:space:]]*.*$|\1- url: \"${ESCAPED_URL}\"|" "$FILE"
else
  sed -i '' -e "${URL_LINE}s|^\([[:space:]]*\)- url:[[:space:]]*.*$|\1- url: \"${ESCAPED_URL}\"|" "$FILE"
fi

# Comment out specific lines
if sed --version >/dev/null 2>&1; then
  sed -i -e "${COMMENT_LINE1}s/^/# /" -e "${COMMENT_LINE2}s/^/# /" "$FILE"
else
  sed -i '' -e "${COMMENT_LINE1}s/^/# /" -e "${COMMENT_LINE2}s/^/# /" "$FILE"
fi
