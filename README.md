
# Rega

<!-- badges: start -->
<!-- badges: end -->

The European Genome-phenome Archive (EGA) provides long-term storage and 
    controlled sharing of personally identifiable genetic data. The Rega 
    package offers a streamlined and extensible R interface to the EGA API, 
    facilitating the programmatic upload of metadata. GEO-like Excel submission
    template is provided as a default method of organizing submission metadata.

## Installation

You can install the development version of `Rega` from github:

``` r
devtools::install_github("ivanek/Rega")
```

## Usage

### Setup credentials

`httr2` is built around the notion that the key should live in an environment 
variable. So the first step is to make your package key available on your local 
development machine by adding a line to your your user-level `.Renviron` 
(which you can easily open with `usethis::edit_r_environ()`)

The `REGA_EGA_PASSWORD` environmental variable isn't supposed to store the 
password in the plain text, instead:
    
- generate a secret key using `httr2::secret_make_key()` and store it as the 
    "REGA_KEY" environmental variable (e.g. using `export` command in bash or
    in `.Renviron` file)
- generate an encrypted password using your secret key via 
    `httr2::secret_encrypt("<your-ega-password>", "REGA_KEY")`
- use the resulting value as "REGA_EGA_PASSWORD"

``` r
Sys.setenv("REGA_EGA_USERNAME" = "<your-ega-username>")
Sys.setenv("REGA_EGA_PASSWORD" = "<your-ega-password>")
```

### Authentication

`Rega` package supports authentication through OAuth or Bearer token via 
functions `ega_oauth` (default) and `ega_token` respectively.

### Data submission

#### Submission metadata parsing

The default parser parses the bundled `xlsx` template. Changing the parser 
behavior, such as whether to append `c4gh` extension to files or which sheets 
to parse from the metadata files is controlled by YAML config file 
`inst/extdata/default_parser_params.yaml`. To modify the parameters, 
create a local copy of the file and pass it as `param_file` argument to 
`default_parser` function.

``` r
library(Rega)

metadata_file <- "<path-to-metadata-file>"

parsed_metadata <- default_parser(metadata_file)
parsed_metadata
```

#### Submission metadata validation

The internal validation is a bit stricter in enforcing uniqueness of titles 
and descriptions for individual tables (the API itself doesn't have this 
requirement). If the validation only fails in this particular case, you can 
still continue with creating the submission. However, there have been instances 
of failed reviews due to non-unique titles and descriptions before.

``` r
validation_summary <- default_validator(parsed_metadata)
validation_summary
```

#### Create API client

Extract EGA API using the bundled YAML specification

``` r
api <- extract_api()
```

Create a client using the embedded `httr2` OAuth authentication (default).

``` r
ega <- create_client(api, verbosity = 0)
```

#### Run new submission workflow

Specify one of the available workflow function with your parsed metadata and 
a client as parameters.

``` r
responses <- new_submission(parsed_metadata, ega, "log.yaml")
```

---

### See vignette for more examples

