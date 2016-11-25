#' Report for PVS
#'
#' Produce standard reports for PVS data
#' @param book Name of the PVS book.
#' @param x Form number on the PVS book.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details bla bla bla.
#' @return It returns automatic reports for the different forms on a PVS book.
#' @examples
#' # bla bla bla
#' pvsrepo(book, 1)
#' @export

repo.pvs <- function(book, x,
                     title = "Automatic report for PVS",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  if (x == 1) {
    fileRmd <- paste(dirfiles, "/rmd/pvs1.Rmd", sep = "")
  }
  if (x == 2) {
    fileRmd <- paste(dirfiles, "/rmd/pvs2.Rmd", sep = "")
  }
  if (x == 3) {
    fileRmd <- paste(dirfiles, "/rmd/pvs3.Rmd", sep = "")
  }
  if (x == 6) {
    fileRmd <- paste(dirfiles, "/rmd/pvs6.Rmd", sep = "")
  }
  if (x == 7) {
    fileRmd <- paste(dirfiles, "/rmd/pvs7.Rmd", sep = "")
  }
  if (x == 9) {
    fileRmd <- paste(dirfiles, "/rmd/pvs9.Rmd", sep = "")
  }
  
  fileURL <- paste(dirfiles, "/rmd/pvs.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/pvs.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/pvs.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(book = book,
                                  x = x,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
