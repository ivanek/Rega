# Rega 0.99.1

* **Initial public release.**
* Provides excel template for filling in submission data
* Implements the core workflow:
  * `default_parser()` – Parses submission data from excel template
  * `create_client()` - Creates API client based on specification
  * `new_submission()` – Submits parsed data to EGA through created API client
* Provides methods for data validation `default_validator()`
* Includes vignette for basic usage
