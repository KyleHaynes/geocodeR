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


# Simple function to standardise
    # TODO - could test timings of \\s vs say " "
st <- function(x){
    x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
}
