#' Rounding Function
#'
#' @param x The number to round
#' @param digits The number of digits to round to.
#'
#' @description
#' Base R Rounding:
#' Note that for rounding off a 5, the IEC 60559 standard (see also ‘IEEE 754’) is expected to be used, ‘go to the even
#' digit’. Therefore round(0.5) is 0 and round(-1.5) is -2. However, this is dependent on OS services and on
#' representation error (since e.g. 0.15 is not represented exactly, the rounding rule applies to the represented number
#' and not to the printed number, and so round(0.15, 1) could be either 0.1 or 0.2).
#'
#' Issue:
#'    This method of rounding is not consistent with rounding from other sources (e.g. Excel) and not inline with our
#'    needs.
#'
#' Solution:
#'    This function gets around this by identifying the specific digit to be rounded by, then applying "ceiling" or
#'    "floor" functions to round up or down as necessary.
#'    This function has been designed to run with both single digits and digit vectors.
#'
#' @examples
#' aj_round2(3.141592654, digits = 3)
#' # [1] 3.142
#'
#' aj_round2(314.1592654, digits = -1)
#' # [1] 310
#' @export
aj_round2 <- function(x, digits = 0) {

    # multiply by 10^digits+1, then check if the last digit is greater than 5.
    z <- (trunc((10 ^ (digits + 1)) * x, digits = digits) %% 10) >= 5

    # Ceiling & Floor only works on whole numbers. multiply by 10^digits to get the decimal in the best spot.
    x <- x * (10 ^ (digits))

    # this is where the magic happens!
    # if <5, use floor
    # if >=5, use ceiling
    output <- z * ceiling(x) + (!z) * floor(x)
    # divide by 10^y again
    output <- output / (10 ^ (digits))

    # return our output
    return(output)
}
