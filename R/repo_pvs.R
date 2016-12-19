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
#' @details It produces a report for different forms of the Mother & Baby methodology
#' implemented by CIP.
#' @return It returns automatic reports for the different forms on a PVS book.
#' @examples
#' repo.pvs(pvsf1, 1)
#' repo.pvs(pvsf2, 2)
#' repo.pvs(pvsf3, 3)
#' repo.pvs(pvsf6, 6)
#' repo.pvs(pvsf7, 7)
#' repo.pvs(pvsf9, 9)
#' @import ggplot2
#' @importFrom factoextra fviz_pca
#' @export

repo.pvs <- function(x, form,
                     title = "Automatic report for PVS",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  if (form == 1) {
    subtitle <- "Identification of selection criteria for genotypes"
    fileRmd <- paste(dirfiles, "/rmd/pvs1.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs1.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs1.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs1.pdf", sep = "")
  }
  if (form == 2) {
    subtitle <- "Best genotypes at flowering"
    fileRmd <- paste(dirfiles, "/rmd/pvs23.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs23.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs23.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs23.pdf", sep = "")
  }
  if (form == 3) {
    subtitle <- "Best genotypes at harvest"
    fileRmd <- paste(dirfiles, "/rmd/pvs23.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs23.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs23.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs23.pdf", sep = "")
  }
  if (form == 6) {
    subtitle <- "Organoleptic analysis at mother trial"
    fileRmd <- paste(dirfiles, "/rmd/pvs67.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs67.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs67.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs67.pdf", sep = "")
  }
  if (form == 7) {
    subtitle <- "Organoleptic analysis at baby trials"
    fileRmd <- paste(dirfiles, "/rmd/pvs67.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs67.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs67.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs67.pdf", sep = "")
  }
  if (form == 9) {
    subtitle <- "Best genotypes at post-harvest"
    fileRmd <- paste(dirfiles, "/rmd/pvs9.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/pvs9.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/pvs9.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/pvs9.pdf", sep = "")
  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(x = x,
                                  form = form,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
