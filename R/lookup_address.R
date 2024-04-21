lookup_address <- function(
    x = c("8 GYMPIE ROAD TIN CAN BAY 4580", "190 MUSGRAVE ROAD RED HILL 4000"),
    deduplicate = TRUE,
    test = FALSE
    # lookup_map = lookup_map
){

    x <- data.table(input = x, row.num = 1:length(x))
    x[, matched := FALSE]
    x[, normalised_input := normalise_fun(input, not_gnaf = TRUE)]


    # ---- Blocking: Address ----
    # First block on address string, this should hopefully weed out a lot.
    block_1 <- merge(x, lookup_map[, .(address_detail_pid, address_label, address, notes)], by.x = "input", by.y = "address", all.x = FALSE)
    # As the threshold has been met, remove from x
    vec <- block_1$normalised_input
    x[normalised_input %chin% vec, matched := TRUE]
    block_1[, matched := TRUE]

    # ---- Create variant addresses ----
    tmp1 <- x[!(matched)][normalised_input %plike% "(\\d+\\w?)\\-(\\d\\w?)"]
    tmp2 <- copy(tmp1)
    # Include both sides of ranged address (1-3 Smith rd, 1 Smith rd, 3 Smith rd)
    tmp1[, normalised_input := gsub("(\\d+\\w?)\\-(\\d\\w?)", "\\1", normalised_input)]
    tmp2[, normalised_input := gsub("(\\d+\\w?)\\-(\\d\\w?)", "\\2", normalised_input)]
    x_sub <- rbind(x[!(matched)], tmp1, tmp2)
    # Now attempt to identify street name. 
    regex <-  "(\\w) \\b(VIEWS|CLS|STREET|CRT|CRES|CRESENT|EST|ACCESS|ALLEY|AVE|ARTERIAL|AVENUE|BEND|BOULEVARD|BRACE|BREAK|BROADWAY|BYPASS|CHASE|CIRCLE|CIRCUIT|CLOSE|CORNER|CORSO|COURT|COVE|CRESCENT|CREST|CROSS|CROSSING|DEVIATION|DRIVE|DRIVEWAY|EASEMENT|ELBOW|END|ENTRANCE|ESPLANADE|FREEWAY|GAP|GARDENS|GATE|GLADE|GREEN|GROVE|HAVEN|HEIGHTS|HIGHWAY|KEY|LANDING|LANE|LINK|LOOP|LYNNE|MALL|MEAD|MEWS|MOTORWAY|PARADE|PASS|PASSAGE|PATHWAY|PLACE|PLAZA|POCKET|POINT|PRECINCT|PROMENADE|RESERVE|REST|RETREAT|RISE|ROAD|ROUTE|ROW|SQUARE|STRAIT|S+T+REET|STRIP|TERRACE|TRACK|TRAIL|VALE|VISTA|WALK|WATERS|WAY|FIRELINE|ACCS|ALLY|ARTL|AV|BVD|BR|BRK|BDWY|BYPA|CH|CIR|CCT|CL|CNR|CSO|CT|CR|CRST|CRSS|CRSG|DE|DR|DVWY|ESMT|ELB|ENT|ESP|FWY|GDNS|GTE|GLDE|GRN|GR|HVN|HTS|HWY|LDG|LYNN|MTWY|PDE|PSGE|PWAY|PL|PLZA|PKT|PNT|PREC|PROM|RES|RTT|RD|RTE|SQ|STAI|ST|STRP|TCE|TRK|TRL|VSTA|WTRS|FLNE)\\b (\\b[A-Z ]*\\b[A-Z ]*\\b\\b[A-Z ]*\\b)"
    x_sub[, short_normalised_input := gsub(regex, "\\1 ", normalised_input, perl = TRUE)]
    x_sub[, short_normalised_input := gsub("[^A-Z]|UNIT|\\bLOT\\b", "", short_normalised_input, perl = TRUE)]

    # And remove.
    rm(tmp1, tmp2)

    # ---- Creates blocks ----
    x_sub <- blocking_fun(x_sub, "normalised_input", not_gnaf = TRUE)[]

    browser(print("heyyy"))

    b <- merge(x_sub[(!matched)], lookup_map[, .(address_detail_pid, address_label, address, short_address, notes, block_3)], by = "block_3", all.x = TRUE, allow.cartesian = TRUE)

    # ---- Blocking: Hashes ----
    # Now block on address string, this should hopefully weed out a lot.
    b <- merge(x_sub[(!matched)], lookup_map[, .(address_detail_pid, address_label, address, short_address, notes, block_1)], by = "block_1", all.x = TRUE, allow.cartesian = TRUE)
    # Identify those that didn't block on anything (these will be added back later).
    no_blocks <- b[is.na(address)]
    if(test){
        message("dataset: `no_blocks` has been created. These are addresses which didn't block.")
        no_blocks <<- no_blocks
    }

    # Second round of blocking
    b2 <- merge(x_sub[row.num %in% no_blocks$row.num], lookup_map[, .(address_detail_pid, address_label, address, short_address, notes, block_2)], by = "block_2", all.x = TRUE, allow.cartesian=TRUE)
    b2 <- b2[!is.na(address), ]


    # Remove non-blocks.
    b <- b[!is.na(address)]

    # rbind them, and remove r2.
    b <- rbind(b,b2)
    rm(b2)


    # ---- String Similarities ----
    b[, jaccard_2_grams := round(stringdist::stringdist(normalised_input, address, method = "jaccard", q = 2), 3)]
    b[, jaccard_1_grams := round(stringdist::stringdist(normalised_input, address, method = "jaccard", q = T), 3)]
    b[, jarowinkler := round(stringdist::stringdist(normalised_input, address, method = "jw"), 3)]
    # Match if threshold achieved.
    b[, sum := jarowinkler + jaccard_2_grams]
    
    # We can take strong matches out.
    block_2 <- b[l <<- sum <= .04]
    block_2[, matched := TRUE]
    # Now remove them from b.
    b <- b[!l]
    # And take our row.nums from b (coz we already have a match).
    vec <- unique(block_2$row.num)
    b <- b[!row.num %in% vec]


    # Within groups, identify weaker members.
    b[!is.na(address_detail_pid), drop := {
        jaccard_2_grams > min(jaccard_2_grams) + .07 &
        jarowinkler > min(jarowinkler) + .07 &
        sum > min(sum) + .13
    }, by = "row.num"]
    # Drop them.
    b <- b[drop != TRUE]
    # Drop the drop.
    b[, drop := NULL]

    # Identify those that are certainly matches.
    b[, matched := fifelse(jaccard_2_grams < .08 & jarowinkler < .08 & sum <= .09, TRUE, FALSE)]

    if(test){
        message("dataset: `all_should_match` has been created, these should all be true positives.")
        all_should_match <<- b[(matched)]
    }

    # Identify those that are terrible and remove.
    b[l <<- !(matched) & jarowinkler > .2 & jaccard_2_grams > .4 & sum > .660]
    if(test){
        message("dataset: `all_should_not_match` has been created, these should all be false positives.")
        all_should_not_match <<- b[(l)]
    }
    # And drop.
    b <- b[!l]
    

    b[(!matched), short_jarowinkler := stringdist::stringdist(short_normalised_input, short_address, method = "jw")]
    
    # Now see if the string is detected on each side    
    b[(!matched) & !is.na(short_normalised_input) & short_normalised_input != "" & short_address != "", string_match := 
        stringi::stri_detect_fixed(
            short_normalised_input,
            short_address,
            negate = FALSE,
            max_count = -1,
            opts_fixed = NULL
        )
    ]
    # The vast majority of time these will be correct
    b[(string_match), matched := TRUE]
    b[(!matched) & (!string_match) & !is.na(short_normalised_input) & short_normalised_input != "" & short_address != "", string_match := 
        stringi::stri_detect_fixed(
            short_address,
            short_normalised_input,
            negate = FALSE,
            max_count = -1,
            opts_fixed = NULL
        )
    ]
    b[(string_match), matched := TRUE]


    b[, jw_split := jw_split_words(normalised_input, address)]

    # View_excel(b[(!string_match)][order(sum_short)])

    # If matched already, import short jaro as the jaro.
    b[(matched), `:=`(short_jarowinkler = jarowinkler)]

    if(test){
        message("dataset: `all_should_match_2` has been created, these should all be true positives.")
        all_should_match_2 <<- b[(!matched) & short_jarowinkler < .03]
    }
    b[(!matched) & short_jarowinkler < .03, matched := TRUE]


    b[, sum_short := short_jarowinkler + jaccard_2_grams]
    b[sum_short < sum & is.na(sum_short), sum := sum_short]


    # Drop bad matches.
    b[l <<- !(matched) & jarowinkler > .2 & jaccard_2_grams > .3 & sum > .580 & short_jarowinkler > .07]
    if(test){
        message("dataset: `all_should_not_match_2` has been created, these should all be false positives.")
        all_should_not_match_2 <<- b[(l)]
    }
    b <- b[!l]


    # ---- Thresholds ----
    # b <- b[(l <<- jaccard_2_grams < .10 | jarowinkler < .10 | (matched)), ]

    # ---- Deduplicate ----
    b[is.na(short_jarowinkler), short_jarowinkler := jarowinkler]
    b <- b[order(sum_short)]
    
    # b[row.num %in% b[duplicated(row.num)]$row.num, {.SD; browser()}, row.num]

    if(deduplicate){
        b <- b[!duplicated(row.num)]
    }

    # b[, row.num := NULL]
    b <- rbind(block_1, block_2, b, no_blocks, fill = TRUE)
    
    # Hrmmm I don't think this dedupe is required...?
    if(deduplicate) {
        tmp1 <- nrow(b)
        b <- b[!duplicated(row.num)]
        # if(nrow(b) < tmp1){
        #     message("kyle this dedupe is apparently required. This msg shouldn't exist in the FINAL version")
        # }
    }
    b[, row.num := NULL]


    # ---- Merge ----
    x <- merge(x[, .(input, row.num)], b, all.x = TRUE, by = "input", allow.cartesian = TRUE)

    if(deduplicate) {
        x <- x[!duplicated(row.num)]
    }

    x <- x[order(row.num)]

    # Some final matches
    x[(!matched) & jaccard_2_grams == 0, matched := TRUE]
    x[(!matched) & sum_short <= .30, matched := TRUE]
    x[matched %in% NA, notes := "Did not block"]
    x[matched %in% NA, matched := FALSE]
    
    # Remove some False positives
    # bc(x[
    #     (matched) & !is.na(short_normalised_input) & 
    #     short_normalised_input != short_address &
    #     short_normalised_input %in% gsub("[^A-Z]", "", getOption("gnaf_list")$normalised_street_names_freq$normalise_fun) &
    #     short_address %in% gsub("[^A-Z]", "", getOption("gnaf_list")$normalised_street_names_freq$normalise_fun)
    # ])

    verbose <- TRUE

    x[l <<- (matched) & !is.na(address), tmp := paste(normalised_input, address)]

    for(i in 1:length(false_pos)){
        # browser()
        x[l & grepl(names(false_pos)[i], tmp, perl = TRUE) & grepl((false_pos)[i], tmp, perl = TRUE), matched := FALSE]
        if(verbose) print(x[l & grepl(names(false_pos)[i], tmp, perl = TRUE) & grepl((false_pos)[i], tmp, perl = TRUE), .(normalised_input, address)])
    }

    return(x[])
}

