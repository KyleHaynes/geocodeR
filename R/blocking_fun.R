blocking_fun <- function(x, address = "input", not_gnaf = FALSE){
    x <- copy(x)
    # TODO: Ensure none of the hashing variables exist
    # TODO: x must be a data.table
    x[, block_1 := st(gsub("\\D", " ", get(..address), perl = TRUE))]
    x[, block_2 := substring(block_1, 0, nchar(block_1) - 2)]
    if(not_gnaf == FALSE){
        x[, block_3 := paste(gsub("([\\d ])\\d$", "\\1", block_1, perl = T), short_address)]
    } else {
        x[, block_3 := paste(gsub("([\\d ])\\d$", "\\1", block_1, perl = T), short_normalised_input)]
    }
    return(x)
}
