#' @name source_gnaf

#' @title UPDATE

#' @description UPDATE.


#' @param gnaf_core_path A character path to G-NAF Core.

#' @param states A character vector of abbreviated Australian State. Default is \code{"QLD"}.

#' @param verbose A logical argument to determine if the function should be verbose or not. Default is \code{TRUE}.


#' @return Returns G-NAF Core wrangled in a way suitable for matching to.


#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @import data.table
#' @export
source_gnaf <- function(
    gnaf_core_path = "c:/temp/gnaf.qld.csv", # Should be NULL if
    states = c("QLD"),
    verbose = TRUE
){

    message("# Sourcing G-NAF Data from PSV.")
    flush.console()
    g <- fread(gnaf_core_path, colClasses = "character")
    message("# Sourced G-NAF Data From PSV.")
    flush.console()

    # Lower-case variable names.
    setnames(g, tolower)

    # Subset to defined States
    g <- g[state == states]

    # If 0 rows, fail.
    if(nrow(g) == 0){
        stop("You have defined: ", paste(states, collapse = ", "), "These do not exist on the input.")
    }

    G <- copy(g)

    set(g, i = NULL, j = c("date_created", "legal_parcel_id",
    "mb_code", "alias_principal", "principal_pid", "primary_secondary",
    "primary_pid"), value = NULL)

    # Remove state from the label.
    g[, address_label := gsub("\\w+ (\\d+)$", "\\1", address_label, perl = TRUE)]
    # Remove Commas from the label.
    g[, address_label := gsub(",", "", address_label, perl = TRUE)]

    # Remove spaces around hyphens in street_name and locality.
    l <- g$street_name %plike% "\\- | \\-"
    if(any(l)){
        message("There are n=", sum(l), " occureces of spaces around hyphens in `street_name`, removing." )
        g[l, street_name := gsub("\\s*\\-\\s*", "-", street_name, perl = TRUE)]
    } else {
        message("# No spaces around hyphens in `street_name`")
    }

    l <- g$locality_name %plike% "\\- | \\-"
    if(any(l)){
        message("There are n=", sum(l), " occureces of spaces around hyphens in `locality_name`, removing." )
        g[l, locality_name := gsub("\\s*\\-\\s*", "-", locality_name, perl = TRUE)]
    } else {
        message("# There no spaces around hyphens in `locality_name`. This is Unexpected (if reading G-NAF from source)")
    }

    # Create a list of characteristics we may want to access later.
    opts <- list(
        street_names_freq =     g[, .N, street_name],
        street_types_freq =     g[, .N, street_type],
        locality_names_freq =   g[, .N, locality_name],
        postcodes_freq =        g[, .N, postcode],
        flat_types_freq =       g[, .N, flat_type],
        normalised_street_names_freq =     g[, .N, normalise_fun(street_name, not_gnaf = TRUE)],
        normalised_street_types_freq =     g[, .N, normalise_fun(street_type, not_gnaf = TRUE)],
        normalised_locality_names_freq =   g[, .N, normalise_fun(locality_name, not_gnaf = TRUE)],
        normalised_postcodes_freq =        g[, .N, normalise_fun(postcode, not_gnaf = TRUE)],
        normalised_flat_types_freq =       g[, .N, normalise_fun(flat_type, not_gnaf = TRUE)],
        normalised_street_names_tokens =      data.table(toks = unlist(stringi::stri_split_regex(normalise_fun(unique(g$street_name), not_gnaf = T), "[^A-Z]", omit_empty = TRUE), use.names = FALSE)), 
        normalised_street_types_tokens =      data.table(toks = unlist(stringi::stri_split_regex(normalise_fun(unique(g$street_type), not_gnaf = T), "[^A-Z]", omit_empty = TRUE), use.names = FALSE)), 
        normalised_locality_names_tokens =    data.table(toks = unlist(stringi::stri_split_regex(normalise_fun(unique(g$locality_name), not_gnaf = T), "[^A-Z]", omit_empty = TRUE), use.names = FALSE)), 
        normalised_flat_types_tokens =        data.table(toks = unlist(stringi::stri_split_regex(normalise_fun(unique(g$flat_type), not_gnaf = T), "[^A-Z]", omit_empty = TRUE), use.names = FALSE))
    )
    # Set these as options.
    options(gnaf_list = opts)

    # ---- Tests ----
    # This code has assumptions that G-NAF is consistent, that may not always be the case. So added
    # some tests.
    # l <- g$street_name %plike% "\\-.*\\-"
    # if(any(l)){
    #     message("# There are n=", sum(l), " occureces of spaces around hyphens in `locality_name`, removing." )
    #     g[l, locality_name := gsub("\\s*\\-\\s*", "-", locality_name)]
    # } else {
    #     message("# There are no spaces around hyphens in `locality_name`")
    # }


    # U1/110 Musgrave Rd Red Hill 4000 QLD
    # UNIT 1/110 Musgrave Rd Red Hill 4000 QLD
    # 1/110 Musgrave Rd Red Hill 4000 QLD
    # 1 110 Musgrave Rd Red Hill 4000 QLD
    if(verbose) message("# Number of unique addresses on input: ", nrow(g))

    lots_only <- g$number_first == "" & g$lot_number != ""
    if(verbose) message("# Number of lots only: ", sum(lots_only))

    units <- g$flat_number != ""
    if(verbose) message("# Number of units (sub-dwellings): ", sum(units))

    ranged <- g$number_last != ""
    if(verbose) message("# Number of street ranged address (e.g. 1-3): ", sum(ranged))

    hyphen_street_name <- g$street_name %flike% "-"
    if(verbose) message("# Number of hyphened street names (e.g. TAMBORINE-OXENFORD): ", sum(hyphen_street_name))

    # TO-DO
    # Should add in one attempt at levels, just to get them in blocking?

    # Original
    # regex <- "\\b(ACCESS|ALLEY|ARTERIAL|AVENUE|BAY|BEACH|BEND|BOULEVARD|BRACE|BREAK|BROADWAY|BYPASS|CHASE|CIRCLE|CIRCUIT|CLOSE|CORNER|CORSO|COURT|COVE|CRESCENT|CREST|CROSS|CROSSING|DEVIATION|DRIVE|DRIVEWAY|EASEMENT|ELBOW|END|ENTRANCE|ESPLANADE|FAIRWAY|FREEWAY|GAP|GARDENS|GATE|GLADE|GLEN|GREEN|GROVE|HAVEN|HEIGHTS|HIGHWAY|HILL|ISLAND|KEY|LANDING|LANE|LINK|LOOP|LYNNE|MALL|MEAD|MEWS|MOTORWAY|OUTLOOK|PARADE|PARK|PARKWAY|PASS|PASSAGE|PATHWAY|PLACE|PLAZA|POCKET|POINT|PRECINCT|PROMENADE|QUAY|RESERVE|REST|RETREAT|RIDGE|RISE|RIVER|ROAD|ROUTE|ROW|SQUARE|STRAIT|STREET|STRIP|TERRACE|TRACK|TRAIL|VALE|VIEW|VISTA|WALK|WATERS|WAY|FIRELINE|HARBOUR|ACCS|ALLY|ARTL|AV|BCH|BVD|BR|BRK|BDWY|BYPA|CH|CIR|CCT|CL|CNR|CSO|CT|CR|CRST|CRSS|CRSG|DE|DR|DVWY|ESMT|ELB|ENT|ESP|FAWY|FWY|GDNS|GTE|GLDE|GRN|GR|HVN|HTS|HWY|ID|LDG|LYNN|MTWY|OTLK|PDE|PWY|PSGE|PWAY|PL|PLZA|PKT|PNT|PREC|PROM|QY|RES|RTT|RDGE|RVR|RD|RTE|SQ|STAI|ST|STRP|TCE|TRK|TRL|VSTA|WTRS|FLNE|HRBR)\\b" 
    # Removing PARK|RIVER|HILL|BEACH|HARBOUR|RIDGE|FAIRWAY|VIEW|BAY|QUAY|OUTLOOK|ISLAND
    ### DONT NEED TO EDIT THIS ONE
    ### DONT NEED TO EDIT THIS ONE
    ### DONT NEED TO EDIT THIS ONE
    regex <-  "(\\w) \\b(ACCESS|ALLEY|AVE|ARTERIAL|AVENUE|CRT|BEND|BOULEVARD|BRACE|BREAK|BROADWAY|BYPASS|CHASE|CIRCLE|CIRCUIT|CLOSE|CORNER|CORSO|COURT|COVE|CRESCENT|CREST|CROSS|CROSSING|DEVIATION|DRIVE|DRIVEWAY|EASEMENT|ELBOW|END|ENTRANCE|ESPLANADE|FREEWAY|GAP|GARDENS|GATE|GLADE|GREEN|GROVE|HAVEN|HEIGHTS|HIGHWAY|KEY|LANDING|LANE|LINK|LOOP|LYNNE|MALL|MEAD|MEWS|MOTORWAY|PARADE|PASS|PASSAGE|PATHWAY|PLACE|PLAZA|POCKET|POINT|PRECINCT|PROMENADE|RESERVE|REST|RETREAT|RISE|ROAD|ROUTE|ROW|SQUARE|STRAIT|S+T+REET|STRIP|TERRACE|TRACK|TRAIL|VALE|VISTA|WALK|WATERS|WAY|FIRELINE|ACCS|ALLY|ARTL|AV|BVD|BR|BRK|BDWY|BYPA|CH|CIR|CCT|CL|CNR|CSO|CT|CR|CRST|CRSS|CRSG|DE|DR|DVWY|ESMT|ELB|ENT|ESP|FWY|GDNS|GTE|GLDE|GRN|GR|HVN|HTS|HWY|LDG|LYNN|MTWY|PDE|PSGE|PWAY|PL|PLZA|PKT|PNT|PREC|PROM|RES|RTT|RD|RTE|SQ|STAI|ST|STRP|TCE|TRK|TRL|VSTA|WTRS|FLNE)\\b (\\b[A-Z ]*\\b[A-Z ]*\\b\\b[A-Z ]*\\b)"

        # Notes:
        # - `fifelse` is marginally quicker than ifelse in the below (half a second).
        # - could `st` be improved?
        # - can't find faster paste, though could be. 
    g[!lots_only, `:=`( 
            v1 = st(
                paste(
                    flat_number, 
                    fifelse(number_last != "", paste0(number_first, "-", number_last), number_first),
                    street_name,
                    street_type,
                    street_suffix,
                    locality_name,
                    postcode,
                    sep = " "
                )
            )
            , v1_notes = "Correct address (no Lots)"
        )
    ]

    g[lots_only, `:=`( 
            v2 = st(
                paste(
                    flat_number, 
                    "LOT ",
                    lot_number,
                    street_name,
                    street_type,
                    street_suffix,
                    locality_name,
                    postcode,
                    sep = " "
                )
            )
            , v2_notes = "Correct address (Lots, prefixed with 'LOT ')"
        )
    ][lots_only]

    g[ranged, `:=`( 
            v3 = st(
                paste(
                    flat_number, 
                    number_first,
                    street_name,
                    street_type,
                    street_suffix,
                    locality_name,
                    postcode,
                    sep = " "
                )
            )
            , v3_notes = "Correct address (first ranged number, e.g. 1 SMITH ST instead of 1-3 SMITH ST)"
        )
    ][ranged]

    g[ranged, `:=`( 
            v4 = st(
                paste(
                    flat_number, 
                    number_last,
                    street_name,
                    street_type,
                    street_suffix,
                    locality_name,
                    postcode,
                    sep = " "
                )
            )
            , v4_notes = "Correct address (last ranged number, e.g. 3 SMITH ST instead of 1-3 SMITH ST)"
        )
    ][ranged]

    d <- data.table(
        address_detail_pid = g$address_detail_pid[!lots_only],
        address_label = g$address_label[!lots_only],
        address = g$v1[!lots_only],
        notes = g$v1_notes[!lots_only],
        geocode_type = g$geocode_type[!lots_only],
        longitude = g$longitude[!lots_only],
        latitude = g$latitude[!lots_only],
        street_name = g$street_name[!lots_only],
        keep.rownames = FALSE
    )

    d <- rbind(
        d,
        data.table(
            address_detail_pid = g$address_detail_pid,
            address_label = g$address_label,
            address = g$address_label,
            geocode_type = g$geocode_type,
            longitude = g$longitude,
            latitude = g$latitude,
            street_name = g$street_name,
            notes = "GNAF Address Label",
            keep.rownames = FALSE
        ),
        data.table(
            address_detail_pid = g$address_detail_pid[lots_only],
            address_label = g$address_label[lots_only],
            address = g$v2[lots_only],
            geocode_type = g$geocode_type[lots_only],
            longitude = g$longitude[lots_only],
            latitude = g$latitude[lots_only],
            street_name = g$street_name[lots_only],
            notes = g$v2_notes[lots_only],
            keep.rownames = FALSE
        ),
        data.table(
            address_detail_pid = g$address_detail_pid[ranged],
            address_label = g$address_label[ranged],
            address = g$v3[ranged],
            geocode_type = g$geocode_type[ranged],
            longitude = g$longitude[ranged],
            latitude = g$latitude[ranged],
            street_name = g$street_name[ranged],
            notes = g$v3_notes[ranged],
            keep.rownames = FALSE
        ),
            data.table(
            address_detail_pid = g$address_detail_pid[ranged],
            address_label = g$address_label[ranged],
            address = g$v4[ranged],
            geocode_type = g$geocode_type[ranged],
            longitude = g$longitude[ranged],
            latitude = g$latitude[ranged],
            street_name = g$street_name[ranged],
            notes = g$v4_notes[ranged],
            keep.rownames = FALSE
        )
    )

    # ---- Normalisation / Standardisation ----
    d[, address := normalise_fun(address)]

    # Add the no locality variant.
    d[, short_address := gsub(regex, "\\1 ", address, perl = TRUE)]
    d[, short_address := gsub("[^A-Z]|UNIT", "", short_address, perl = TRUE)]

    # ---- Create blocks on G-NAF ----
    d <- blocking_fun(d, "address")

    varss <- unique(c("address_detail_pid", "street_name", names(G)[!names(G) %in% names(d)]))
    varss <- varss[varss %in% names(G)]

    return(
        list(
            lookup_map  = d[],
            gnaf_full   = G[, ..varss]
        ))
}
