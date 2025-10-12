# geocodeR: Australian Address Matching and Geocoding

**IN DEVELOPMENT**

<!-- badges: start -->
[![R-CMD-check](https://github.com/KyleHaynes/au.address.match/workflows/R-CMD-check/badge.svg)](https://github.com/KyleHaynes/au.address.match/actions)
[![Codecov test coverage](https://codecov.io/gh/KyleHaynes/au.address.match/branch/main/graph/badge.svg)](https://codecov.io/gh/KyleHaynes/au.address.match?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/geocodeR)](https://CRAN.R-project.org/package=geocodeR)
<!-- badges: end -->

geocodeR provides comprehensive tools for matching and geocoding Australian addresses against the Geocoded National Address File (G-NAF). The package includes advanced fuzzy string matching algorithms, address normalization functions, and an interactive Shiny application for address geocoding and visualization.

## Features

- **High-performance address matching** using multiple blocking strategies
- **Fuzzy string matching** with Jaro-Winkler, Jaccard, and custom similarity metrics
- **Address normalization** for Australian address formats
- **Interactive Shiny application** for geocoding and visualization
- **Batch processing** capabilities for large datasets
- **Spatial analysis** integration with sf objects

## Installation

You can install the development version of geocodeR from GitHub:

```r
# install.packages("remotes")
remotes::install_github("KyleHaynes/au.address.match")
```

## Quick Start

### Basic Address Matching

```r
library(geocodeR)

# First, prepare your G-NAF lookup data
lookup_map <- source_gnaf("path/to/gnaf_core.psv", states = "QLD")

# Match addresses
addresses <- c(
  "120 Musgrave Road, Red Hill QLD 4059",
  "Unit 5, 123 Queen Street, Brisbane QLD 4000"
)

results <- lookup_address(addresses)
print(results)
```

### Launching the Shiny Application

```r
# Launch interactive geocoding application
geocodeR()
```

## Main Functions

### `source_gnaf()`
Prepares G-NAF data for address matching by creating normalized address variants and blocking indexes.

### `lookup_address()`
Performs fuzzy address matching against the prepared G-NAF dataset using multiple similarity metrics.

### `geocodeR()`
Launches an interactive Shiny application for address geocoding, visualization, and analysis.

## Address Matching Process

The package uses a sophisticated multi-stage matching process:

1. **Exact Matching**: Direct string matches for perfect addresses
2. **Hierarchical Blocking**: Multiple blocking strategies to reduce search space
3. **Fuzzy Matching**: Jaro-Winkler, Jaccard, and composite similarity scores
4. **False Positive Filtering**: Built-in filters to reduce incorrect matches

## Data Requirements

This package requires G-NAF data, which can be downloaded from:
- [data.gov.au](https://geoscape.com.au/solutions/g-naf/)

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
