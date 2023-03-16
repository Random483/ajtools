#' Get Filename
#'
#' @param filename The filename used (excluding version control and file extension).
#' @param output_folder The folder where the outputs should be saved.
#' @param extension I'm using .csv and/or .xlsx, but can be anything.
#' @usage
#' aj_input_file(
#'     filename,
#'     output_folder = "./",
#'     extension = "xlsx"
#' )
#' @description
#' Allows the user to define a filename with version control.
#' The latest version of said file is then identified and returned.
#' This is the sister function to `aj_output_file()` and looks for files using that naming convention.
#' @examples
#' list.files(path = "./outputs/")
#' # [1] output1_v1.0.xlsx  output1_v1.1.xlsx  output1_v1.2.xlsx  output2_v1.0.xlsx
#' 
#' aj_input_file(
#'     filename = "output1",
#'     output_folder = "./outputs/",
#'     extension = "xlsx"
#' )
#' # [1] "./outputs/output1_v1.2.xlsx"
#' @export
aj_input_file <- function(
    filename,
    output_folder = "./",
    extension = "xlsx"
) {
    # create regex string for file name, including file name, extension and wildcard for version number
    fn <- paste(
        "^", filename, # start of filename
        ".*", #wildcard for middle
        "\\.", extension, "$", # end of filename
        sep = ""
        )

    # get list of files
    n1 <- list.files(path = output_folder, pattern = fn, ignore.case = TRUE)

    # extract version number from filenames
    n2 <- gsub(paste(filename, "_v", sep = ""), "", n1)
    n2 <- gsub(paste(".", extension, sep = ""), "", n2)
    n2 <- as.numeric(n2)

    # file with latest version number
    n3 <- paste(
        output_folder, # path
        n1[which.max(n2)], # filename
        sep = ""
    )

    # return file
    return(n3)
}
