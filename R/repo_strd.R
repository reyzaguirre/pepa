#' Automatic report for a strip plot design
#'
#' Produces an automatic report for selected variables in an experiment
#' with a strip plot design.
#' @param dfr The name of the data frame.
#' @param vars The variables to analize.
#' @param rowf The row factor.
#' @param colf The column factor.
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
#' @details It fits a linear model for a strip plot design and explains the results.
#' @return It returns an explanation about the strip plot design fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' # Create design
#' rowf <- paste0("rf", 1:3)
#' colf <- paste0("cf", 1:4)
#' design <- cr.strd(rowf, colf, 3)$book
#' # Some random data
#' design$yield <- rnorm(36, 10)
#' repo.strd(design, "yield", "A", "B", "block")
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.strd <- function(dfr, vars, rowf, colf, rep,
                      title = "Automatic report for a strip plot design",
                      subtitle = NULL,
                      author = "International Potato Center",
                      format = c("html", "word", "pdf"),
                      server = FALSE,
                      server_dir_name = "directory",
                      server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/strd.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/strd.html")
    fileDOCX <- paste0(dirfiles, "/rmd/strd.docx")
    filePDF <- paste0(dirfiles, "/rmd/strd.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "strd.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "strd.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  rowf = rowf,
                                  colf = colf,
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
