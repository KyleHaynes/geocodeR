#' Jaro-Winkler string similarity with word matching
#'
#' @param vec_1 Character vector of first strings
#' @param vec_2 Character vector of second strings  
#' @param type Character indicating type of matching (1, 2, or 3)
#' @param allow_contraction Logical allowing word contractions
#'
#' @return Numeric vector of similarity scores
#' @export
jwink <- function(vec_1 = c("kyle john", "k yle j")
                , vec_2 = c("kyle j", "kyle j")
                , type = "1"
                , allow_contraction = TRUE
                ){
    vec_1 <- strsplit(as.character(st(vec_1)), " ", fixed = TRUE)
    vec_2 <- strsplit(as.character(st(vec_2)), " ", fixed = TRUE)

    if(allow_contraction){
        vec_1 <- lapply(vec_1, function(x) {c(x, paste(x, collapse = ""))})
        vec_2 <- lapply(vec_2, function(x) {c(x, paste(x, collapse = ""))})
    }

    if(type == "1"){
        mapply(function(x ,y) sum(sort(c(outters <- outer(x, y, funfun)), decreasing = T)[1:(min(dim(outters)) -> dims)]) / dims, vec_1, vec_2)
    } else if(type == "2") {
        mapply(function(x , y) max(outer(x, y, funfun)), vec_1, vec_2)
    } else if(type == "3") {
        mapply(function(x , y) any(stringr::str_detect(c(x,y), c(y, x))), vec_1, vec_2)
        # str_detect(TallestPerson, President) mapply(function(x , y) browser(), vec_1, vec_2)
    }
}

#' Jaro-Winkler similarity for word-split strings
#'
#' @param vec_1 Character vector of first strings
#' @param vec_2 Character vector of second strings
#' @param regex Regular expression for splitting (default "[^A-Z]")
#'
#' @return Numeric vector of Jaro-Winkler similarity scores
#' @export
jw_split_words <- function(vec_1, vec_2, regex = "[^A-Z]") {
# browser()
    vec_1 <- stringi::stri_split_regex(
        vec_1,
        regex,
        omit_empty = F
    )

    vec_2 <- stringi::stri_split_regex(
        vec_2,
        regex,
        omit_empty = F
    )

    # Define a function to compute Jaro-Winkler similarity
    compute_jw <- function(x, y) {
        # Create the matrix
        smatrix <- round(stringdist::stringdistmatrix(unique(x), unique(y), method = "jw"), 3)

        # Determine how many we want
        threshold <- fifelse(ncol(smatrix) < nrow(smatrix), ncol(smatrix), nrow(smatrix))
        threshold <- fcase(
            threshold %between% c(1,2), threshold - 0, 
            threshold %between% c(3,5), threshold - 1, 
            threshold %between% c(6,9), threshold - 1, 
            threshold %between% c(10,100), threshold - 2
        )

        threshold <- sum(sort(apply(smatrix, fifelse(ncol(smatrix) < nrow(smatrix), 2, 1), min))[1:threshold]) / threshold
    }


    # Define a function to compute Jaro-Winkler similarity

    # Apply the function to corresponding elements of the two vectors
    jw_scores <- mapply(compute_jw, vec_1, vec_2)

    # Return scores
    jw_scores
}
