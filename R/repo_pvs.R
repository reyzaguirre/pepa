#' Report for PVS
#'
#' Produce standard reports for PVS data
#' @param x A data frame with data for a pvs form.
#' @param form Form number.
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
#' repo.pvs(pvsf1, 1)
#' @export

repo.pvs <- function(x, form,
                     title = "Automatic report for PVS",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  if (form == 1) {
    subtitle <- "Selection criteria for clones"
    fileRmd <- paste(dirfiles, "/rmd/pvs1.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs1.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs1.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs1.pdf", sep = "")
  }
  if (form == 2) {
    subtitle <- "Best clones at flowering"
    fileRmd <- paste(dirfiles, "/rmd/pvs2.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs2.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs2.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs2.pdf", sep = "")
  }
  if (form == 3) {
    subtitle <- "Best clones at harvest"
    fileRmd <- paste(dirfiles, "/rmd/pvs3.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs3.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs3.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs3.pdf", sep = "")
  }
  if (form == 6) {
    subtitle <- "Organoleptic analysis at mother trial"
    fileRmd <- paste(dirfiles, "/rmd/pvs6.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs6.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs6.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs6.pdf", sep = "")
  }
  if (form == 7) {
    subtitle <- "Organoleptic analysis at baby trials"
    fileRmd <- paste(dirfiles, "/rmd/pvs7.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs7.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs7.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs7.pdf", sep = "")
  }
  if (form == 9) {
    subtitle <- "Best clones at post-harvest"
    fileRmd <- paste(dirfiles, "/rmd/pvs9.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs9.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs9.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs9.pdf", sep = "")
  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(x = x,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
