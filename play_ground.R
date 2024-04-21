

# ---- PLAY GROUND ----
if(FALSE){
# ---- SETUP ----
require(data.table)

options(max.print = 3000)
source("./R/blocking_fun.R")
source("./R/false_positive_named_vector.R")
source("./R/lookup_address.R")
source("./R/normalisation_funs.R")
source("./R/source_gnaf.R")
source("./R/winkler_funs.R")

# devtools::document()

lookup_map <- source_gnaf()

lookup_address(vec)

# lookup_map <- source_gnaf()
x = lookup_address(d$a %sample% 10000)
system.time({
    x = lookup_address(d$a)
})




# ---- INSPECTING x ----
view_excel(x[order(sum)][jaccard_2_grams >= 0.05 & short_jarowinkler >= .05, .(address_label, normalised_input, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)])

x[, jdiff := short_jarowinkler / jarowinkler]


x[!is.na(short_jarowinkler) & sum > 0][order(short_jarowinkler)]
bc <- function(x){

    x = copy(x)
 
    vars <- unique(c("address", "normalised_input", "sum", "jarowinkler", "jaccard_1_grams", "jaccard_2_grams", "short_address", "short_normalised_input", "sum_short", "short_jarowinkler", names(x)))
    vars <- vars[vars %in% names(x)]
    setcolorder(x, vars)

    vars <- vars[!vars %in% c("row.num",
        "block_1",
        "block_2",
        "input")]


    # vars <- vars[!vars %in% c("normalised_input")]
    x1 <- x[, ..vars]
    x1[, normalised_input := NULL]
    x2 <- x[, ..vars]
    x2[, address := NULL]
    # browser()
    # browser()
    setnames(x2, "normalised_input", "address")

    internal::beyond_compare(
        x1,
        x2,
        row_names = TRUE
    )
}

# 2 ELEVENTH AVENUE SCOTTVILLE 4804
# 2 SEVENTH AVENUE SCOTTVILLE 4804
23 MANNING STREET STANTHORPE 4380
# RAINS
# HAINS
# MORT
# NORTH

# 24 JOSEPHFINA COURT LOGAN RESERVE 4133
# 24 JOSEPHINE COURT LOGAN RESERVE 4133



#
vec <- c(
"31A MAITLAND ROAD BURPENGARY EAST 4505",
"1 LORA CLOSE EDMONTON 4869",
"19 DAMPER COURT JIMBOOMBA 4280",
"28 LILY STREET GYMPIE 4570",
"5 KINNEAR ST CHILLAGOE 4871", # Maybe doesn't exist
"2 SENTINEL DRIVE GREENBANK 4124",
"9 MC CANN STREET EDMONTON 4869"
)
set.seed(1)
x <- lookup_address(c(d$a))


jwink


d <- readRDS("Z:/Project/QGSO/Data Integration/QHF/QPF/frame updates/2024-01 frame creation/output/2024-01_di_qpf_master_frame.rds")




x[normalised_input %like% "27 SANTLEY STREET MT GRAVATT 4122"]

jaccard_2_grams <= 0.05 & short_jarowinkler >= .3

beyond_compare(
    x[order(sum)][jaccard_2_grams >= 0.05 & short_jarowinkler >= .3, .(add = address_label, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)],
    x[order(sum)][jaccard_2_grams >= 0.05 & short_jarowinkler >= .3, .(add = normalised_input, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)],
    row_names = T
)

view_excel(x[order(sum)][jaccard_2_grams >= 0.05 & short_jarowinkler >= .3, .(address_label, normalised_input, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)])

beyond_compare(
    x[order(sum)][notes %nin% c("Correct address (no Lots)", "GNAF Address Label", NA), .(add = address, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)],
    x[order(sum)][notes %nin% c("Correct address (no Lots)", "GNAF Address Label", NA), .(add = normalised_input, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)],
    row_names = T
)

view_excel(x[order(sum)][notes %nin% c("Correct address (no Lots)", "GNAF Address Label", NA), .(normalised_input, address_label, sum,  jarowinkler, jaccard_2_grams, short_address, short_normalised_input, sum_short, short_jarowinkler)])

source("C:\\temp\\2024_address_matching.R")
lookup_address(c("5-7 JUDITH STREET RUSSELL ISLAND 4184", "UNIT 8 24 ROGOONA STREET MORNINGSIDE 4170", "5 DAPSANG DRIVE TAMBORINE MT 4272"))

vec <- c(
"31A MAITLAND ROAD BURPENGARY EAST 4505",
"1 LORA CLOSE EDMONTON 4869",
"19 DAMPER COURT JIMBOOMBA 4280",
"28 LILY STREET GYMPIE 4570",
"5 KINNEAR ST CHILLAGOE 4871", # Maybe doesn't exist
"2 SENTINEL DRIVE GREENBANK 4124",
"9 MC CANN STREET EDMONTON 4869"
)
x <- lookup_address(vec, deduplicate = T)
x <- fread("Z:/Project/QGSO/Data Integration/QHF/Data (supplementary)/GNAF/G-NAF FEBRUARY 2022/Authority Code/Authority_Code_STREET_TYPE_AUT_psv.psv")

tmp <- dput(data.frame(x))


vec <- c(
      "UNIT 4 1-5 NORMA ST NEW AUCKLAND 4680"
    , "31A MAITLAND ROAD BURPENGARY EAST 4505"
    , "126 NORTH STREET TOOWOOMBA 4350"
    , "69 SENTINEL DRIVE GREENBANK 4124"
    , "29 ARTHER STREET GAYNDAH 4625"
    , "27 CATHY COURT ELIMBAH 4516"
    , "8 PERRY STREET BURPENGARY 4505"
    , "27CONEFLOWER STREET CABOOLTURE 4510"
    , "21 WOODROW STREET BEENLEIGH 4207"
    , "52 GAILEE COURT JIMBOOMBA 4280"
    , "36 MT DEBATABLE ROAD GAYNDAH 4625"
    , "56 REYNOLDS ROAD CURRUMBIN VALLEY 4223"
    , "12 SHETLAND COURT GREENBANK 4124"
    , "9 CHARLES STREET DECEPTION BAY 4508"
    , "66 THE STRAND TOWNSVILLE 4810"
    , "30 RUATOKA COURT NARANGBA 4504"
    , "LOT 4 HULL HEADS ROAD HULL HEADS 4854" # <--- lol
    , "79 URBAN ROAD ELIMBAH 4516"
)
x <- lookup_address(vec)


# The following now all match.
x <- lookup_address(c(
    "7 GROVE STREET GREENBANK 4124",
    "15 LUCANIA COURT TAMBORINE MT 4272",
    "85 MARY STREET CHARTERS TOWERS 4820",
    "12 MT VISTA COURT MORAYFIELD 4506",
    "27 HATHWAY STREET MT GRAVATT EAST 4122"
))


# "NORTH TAMBORINE 4272" --> "TAMBORINE MOUNTAIN 4272"
# "HORN ISLAND 4875" --> "HORN 4875"
# Does "WONGA 4873" exists? if not, should it be "WONGA BEACH"?

q <- readRDS("Z:/Project/QGSO/Data Integration/QHF/QPF/frame updates/2024-01 frame creation/output/2024-01_scia_qpf_identified_sample_frame.rds")

vec <- q[geocoded_accuracy != 1]$full_physical_address
vec <- st(gsub("\\bQLD\\b|,", " ", vec))
t <- lookup_address(vec)

t <- cbind(q[geocoded_accuracy != 1], t[, ])


# sum = .13114 is the first
# sum >= .31 & short_jarowinker > 0.016 ### wheels are coming off in a big way

}


