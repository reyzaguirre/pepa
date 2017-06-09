#' Automatic report for a North Carolina genetic design.
#'
#' Produces an automatic report for a North Carolina I, II or III genetic design analysis.
#' @param model 1, 2 or 3.
#' @param data A data frame with the appropiate columns. See details
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model to data from a North Carolina I, II or III
#' genetic design.
#' @return It returns an ANOVA and related quantities.
#' @examples
#' library(agricolae)
#' data(DC)
#' carolina1 <- DC$carolina1
#' repo.nc(1, carolina1)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.nc <- function(model = c(1, 2, 3), data,
                    title = "Automatic report for a North Carolina genetic design",
                    subtitle = NULL,
                    author = "International Potato Center",
                    format = c("html", "word", "pdf")) {

  model <- match.arg(model)
  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/rmd/nc.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/nc.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/nc.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/nc.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(model = model,
                                  data = data,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
