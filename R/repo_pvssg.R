#' Report for PVS - Global summary
#'
#' Produce a report for PVS data global summary sheet
#' @param traits A list of traits to be included in the report.
#' @param data A data frame with data for a pvs global summary form.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It produces a report for global summary sheet of the Mother & Baby methodology
#' implemented by CIP.
#' @return It returns an automatic report.
#' @examples
#' traits <- c("NMTP_Mean_mother", "TNTP_Mean_mother", "PPH_Mean_mother", "TTYA_Mean_mother")
#' repo.pvssg(traits, pvssg)
#' @import ggplot2
#' @importFrom factoextra fviz_pca
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @export

repo.pvssg <- function(traits,
                       data,
                       title = "Automatic report for PVS",
                       subtitle = "Global summary data",
                       author = "International Potato Center",
                       format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/rmd/pvssg.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/pvssg.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/pvssg.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/pvssg.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  data = data,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
