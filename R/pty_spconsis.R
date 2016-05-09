#' Pepa tells you about the consistency of your data
#'
#' It checks your data for inconsistencies.
#' @param data The name of the data frame.
#' @param plot.size Plot size in square meters.
#' @param f Factor for extreme values detection. See details.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details The data frame must use the labels (lower or upper case) specified in
#' the function \code{spconsis} of package \code{st4gi}. Type \code{?spconsis} to
#' see the list and for additional details.
#' @return It returns a list of all rows with some kind of inconsistency and
#' all rows with outliers.
#' @examples
#' pty.spconsis(pjpz09, 4.5)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.spconsis <- function(data, plot.size, f = 3,
                         author = "International Potato Center",
                         format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/spconsis.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/spconsis.html", sep = "")
  fileDOCX <- paste(dirfiles, "/spconsis.docx", sep = "")
  filePDF <- paste(dirfiles, "/spconsis.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(data = data,
                                  plot.size = plot.size,
                                  f = f,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
