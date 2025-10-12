#' Normalize Australian addresses for matching
#'
#' @param vec Character vector of addresses to normalize
#' @param additional_regex_from Additional regex patterns to replace (not currently used)
#' @param additional_regex_to Additional replacement patterns (not currently used)
#' @param not_gnaf Logical indicating if input is not from G-NAF (default FALSE)
#'
#' @return Character vector of normalized addresses
#' @export
normalise_fun <- function(vec, additional_regex_from = NULL, additional_regex_to = NULL, not_gnaf = FALSE){
    # Mountains
    v <- copy(vec)
    v <- toupper(v)
    v <- gsub("MOUNTAINS*|MOUNT", "MT", v, perl = TRUE)

    # Saints
    v <- gsub("SAINT", "ST", v, fixed = TRUE)

    # Doctors
    v <- gsub("DOCTOR", "DR", v, fixed = TRUE)

    # 's
    v <- gsub("'", "", v, fixed = TRUE)

    # Ands
    v <- gsub("\\bAND\\b", "&", v, fixed = TRUE)

    # -
    # Removing this, coz extra work on ranges is required.
    # v <- gsub("-", " ", v, fixed = TRUE)

    # People generally don't append CITY after like Brisbane City / Tville City etc.
    v <- gsub(" CITY (\\d+)", " \\1", v, perl = TRUE)

    # Normalise to G-NAF (save time)
    if(not_gnaf){
        # 4271 Doesn't exist on G-NAF
        v <- gsub("4271$", "4272", v, perl = TRUE)

        # Units are not split by / on G-NAF
        v <- gsub("/", " ", v, fixed = TRUE)

        # Units are not split by / on G-NAF
        v <- gsub(",", " ", v, fixed = TRUE)


    } else {
        message("Not doing further normalisation (You should consider setting `not_gnaf = TRUE` this!)")
    }


    return(st(v))
}

#' Standardize whitespace in text
#'
#' @param x Character vector to standardize
#'
#' @return Character vector with standardized whitespace
#' @export
st <- function(x){
    x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
}
