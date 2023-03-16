#' Identical Vectorised
#'
#' this function checks to see if two columns are equal, row by row.
#' It handles cases where NA exists too
aj_identical_vectorised <- function(x, y) {
    a <- (x != y | (is.na(x) | is.na(y))) & !(is.na(x) & is.na(y))
    return(a)
}
