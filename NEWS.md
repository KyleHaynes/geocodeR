# geocodeR 0.1.0

## New Features

* Initial release of geocodeR package
* `source_gnaf()` function for preparing G-NAF data
* `lookup_address()` function for fuzzy address matching
* `geocodeR()` Shiny application for interactive geocoding
* Comprehensive address normalization and blocking strategies
* Support for batch processing of large address datasets

## Functions

* `source_gnaf()` - Load and prepare G-NAF data for matching
* `lookup_address()` - Match addresses against G-NAF with fuzzy matching
* `normalise_fun()` - Normalize Australian addresses
* `blocking_fun()` - Create blocking hashes for efficient searching
* `jw_split_words()` - Calculate Jaro-Winkler similarity on word tokens
* `geocodeR()` - Launch interactive Shiny application

## Dependencies

* Requires R >= 4.0.0
* Core dependencies: data.table, stringdist, stringi
* Shiny app dependencies: shiny, DT, leaflet, sf, bslib
* Optional: fastmatch for improved performance

## Documentation

* Comprehensive README with examples
* Function documentation with roxygen2
* Usage vignette demonstrating workflows
* Unit tests for core functionality
