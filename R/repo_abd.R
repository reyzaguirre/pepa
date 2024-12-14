#' Automatic report for an augmented block design (ABD)
#'
#' Produces an automatic report for selected variables in an experiment with an ABD.
#' @param dfr The name of the data frame.
#' @param vars The variables to analize.
#' @param geno The genotypes.
#' @param rep The replications.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @details It fits a linear model for an ABD and explains the results.
#' @return It returns an explanation about the ABD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' # A subset that looks like an ABD
#' tmp <- pjpz09[c(1, 2, 9, 10, 13, 14, 27, 29, 31, 33, 35, 37, 40, 42, 44, 46, 48, 50, 169, 170), ]
#' repo.abd(tmp, c("trw", "vw"), "geno", "rep")
#' # With some missing values
#' tmp[c(1, 2, 3), "trw"] <- NA
#' tmp[c(1, 10, 15), "vw"] <- NA
#' repo.abd(tmp, c("nocr", "trw", "vw"), "geno", "rep")
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.abd <- function(dfr, vars, geno, rep,
                     title = "Automatic report for an Augmented Block Design (ABD)",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "directory",
                     server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/abd.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/abd.html")
    fileDOCX <- paste0(dirfiles, "/rmd/abd.docx")
    filePDF <- paste0(dirfiles, "/rmd/abd.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "abd.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "abd.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  geno = geno,
                                  rep = rep,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (!server) {

    if (format == "html_document")
      try(browseURL(fileURL))

    if (format == "word_document")
      try(system(paste("open", fileDOCX)))

    if (format == "pdf_document")
      try(system(paste("open", filePDF)))

  } else {

    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)

  }

}
