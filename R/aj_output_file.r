#' Set Filename
#'
#' @param filename The filename used (excluding version control and file extension).
#' @param output_folder The folder where the outputs should be saved.
#' @param major_v The major version number.
#' @param extension I'm using .csv and/or .xlsx, but can be anything.
#' @usage
#' aj_output_file(
#'     filename,
#'     output_folder = "./",
#'     major_v = 0,
#'     extension = "xlsx"
#' )
#' @description
#' Allows the user to define a filename with version control.
#' Minor version increments with each additional file with the defined filename and given major version number.
#' Note: incremental version control breaks with multiple files using the same name.
#' @examples
#' list.files(path = "./outputs/")
#' # [1] output1_v1.0.xlsx  output1_v1.1.xlsx  output1_v1.2.xlsx  output2_v1.0.xlsx
#'
#' aj_output_file(
#'     filename = "output1",
#'     output_folder = "./outputs/",
#'     major_v = 1,
#'     extension = "xlsx"
#' )
#' # [1] "./outputs/output1_v1.3.xlsx"
#'
#' aj_output_file(
#'     filename = "output1",
#'     output_folder = "./outputs/",
#'     major_v = 2,
#'     extension = "xlsx"
#' )
#' # [1] "./outputs/output1_v2.0.xlsx"
#' @export
aj_output_file <- function(
    filename,
    output_folder = "./",
    major_v = 0,
    extension = "xlsx"
) {
    n <- list.files(path = output_folder)
    minor_v <-
        sum(grepl(paste(
            "^", filename, "_v", major_v, # beginning of string
            ".*", # wildcard middle
            ".", extension, # end of srting
            sep = ""
        ), n))
    output <- paste(
        output_folder,
        filename,
        "_v",
        major_v,
        ".",
        minor_v,
        ".",
        extension,
        sep = ""
    )
    return(output)
}
