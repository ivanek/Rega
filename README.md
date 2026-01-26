
# Rega

<!-- badges: start -->
<!-- badges: end -->

The European Genome-phenome Archive (EGA) provides long-term storage and 
    controlled sharing of personally identifiable genetic data. The Rega 
    package offers a streamlined and extensible R interface to the EGA API, 
    facilitating the programmatic upload of metadata. GEO-like Excel submission
    template is provided as a default method of organizing submission metadata.

# Installation

You can install the development version of `Rega` from github:

```r
devtools::install_github("ivanek/Rega")
```


# Load packages

```r
library(Rega)
```

# Setting Up Secure Credentials

The httr2 package follows security best practices by storing sensitive 
information (like API keys or passwords) as environment variables rather 
than hard-coding them into your scripts.

To keep your credentials secure, we use a two-step process:

- Create a Master Key: A secret key to encrypt/decrypt data.
- Encrypt your Password: Storing the password in a format that is unreadable 
    without the Master Key.

## Create and Store a Master Secret Key

```r
# Run this in your R console to generate a key
httr2::secret_make_key()
```
To make this key available every time you open R, you must store it in your 
user-level .Renviron file.

- Run usethis::edit_r_environ() to open the file.
- Add the following line (replace the string with the key you just generated): 
    `REGA_KEY="your-generated-key-here"`
- Save and close the file.

Important: Restart R after saving to ensure the variable is loaded into your 
environment.

## Encrypt Your EGA Password

Now, use your master key (REGA_KEY) to encrypt your actual EGA password. This 
ensures that even if someone sees your .Renviron file, they cannot read your 
password.


```r
# Replace <your-ega-password> with your actual password
# This returns a long encrypted string. Copy it.
httr2::secret_encrypt("<your-ega-password>", "REGA_KEY")
```

## Store the Encrypted Password

Finally, store the encrypted string (not your plain-text password) in your 
.Renviron file.

- Open your .Renviron again: usethis::edit_r_environ().
- Add the encrypted string as a new variable: 
    `REGA_EGA_PASSWORD="your-encrypted-string-here"`
- Save and close.

## Final Step: Restart R

# Fill in the submission template

Download the empty MS Excel template from
`inst/extdata/ega_full_template_v3.xlsx` and fill it in according to the
instructions in the 'Instructions' tab.

# Data submission

## Metadata parsing

The default parser is pre-configured to handle the bundled xlsx template 
(`inst/extdata/ega_full_template_v3.xlsx`) automatically. As long as the 
templateis filled out according to the provided instructions, the default 
parameters will work seamlessly, and no manual adjustments are required.

If you need to customize the parser's behavior—such as toggling the `c4gh` 
file extension, you can modify the settings via the YAML configuration. To do 
this, create a local copy of `inst/extdata/default_parser_params.yaml`,
adjust the values as needed, and pass the path of your new file to the 
`param_file` argument in the `default_parser` function.

```r
metadata_file <- system.file(
    "extdata/submission_example.xlsx",
    package = "Rega"
)

parsed_metadata <- default_parser(metadata_file)
head(parsed_metadata)
```

## Metadata validation

To ensure a seamless submission process, the package includes a client-side 
validation layer. This system automatically cross-references your metadata 
against the schema requirements of both the EGA API and the underlying target 
database. To ensure your submission continues smoothly, you should address all 
flagged validation failures and errors.

```r
validation_summary <- default_validator(parsed_metadata)
validation_summary
```

## Running `new_submission` workflow

```r
responses <- new_submission(parsed_metadata, logfile = "log.yaml")
```

# Manual client creation

If you encounter errors during metadata submission and would like to get more
details, you can create a client with verbose logging.

Extract EGA API using the bundled YAML specification and create a client using 
the embedded `httr2` OAuth authentication (default), changing the verbosity.

```r
api <- extract_api()
ega <- create_client(api, verbosity = 3)
```

Run the `new_submission` workflow with the custom client.

```r
responses <- new_submission(parsed_metadata, client = ega)
```

This will create your metadata submission in EGA and fill in all provided
information. However, this workflow does not finalize your submission. In order
to finalize submission either use the GUI interface of EGA Submitter Portal,
or run `finalise_submission("<returned_submission_id>", "<release_date>")`.
Note that the release date should ideally be around 2 weeks away from 
metadata submission to allow for review by EGA team.

# Other workflows

There are several other workflow available:

- `get_submission`: 
- `get_entry_by_title`: 
- `delete_submission_contents`: 
- `delete_submission`: 
- `rollback_submission`:

Please see the corresponding help pages for more details.

## Examples

You can get the detailed data on individual tables (`submissions`, `studies`, 
`samples`, `experiments`, `runs`, `analyses` and `datasets`) that contain a 
specific string in their `title` column using `get_entry_by_title` function.

```r
# checks all tables
resp <- get_entry_by_title("RNASeq")
# checks only samples and studies, logs responses
resp <- get_entry_by_title(
    "RNASeq", type = c("samples", "studies"), logfile = "log.yaml"
)
```

Or delete the entire contents of current submission metadata via
`delete_submission_contents` workflow or delete the entire submission by
using the `delete_submission` workflow.

```r
resp <- delete_submission_contents(00001, ega)
resp <- delete_submission(00001, ega)
```

**Workflow for updating the submission metadata by `PUT` method is under
development.**

# Utilities

If you wish to create your own templates for EGA submissions, we provide
a few functions to retrieve properties and enums through API and save
them in text files. We will use the API and the client created above.

Relevant functions include:

- `get_schemas()`
- `get_properties()`

# Notes

## Bearer token authentication

For testing, debugging and prototyping purposes, it is possible to
directly use generated bearer token with API when creating the client.
It is then the responsibility of the user to track the validity and
refresh the token as necessary.

```r
bt <- ega_token()
ega <- create_client(api, bt$access_token)

ega$get__enums()
```

# Issues
