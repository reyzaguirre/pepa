#' Pepa tells you
#'
#' Explain an R object in plain English if she knows about it
#' @param x An R object.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It uses a set of templates to explain R objects in plain English.
#' It aims to produce automatic reports for some standard statistical procedures, most
#' of them included in the \code{st4gi} package.
#' @return It returns an explanation about the selected R object.
#' @examples
#' # Pepa tells you something about a data frame:
#' pty(pjpz09)
#' @importFrom utils browseURL
#' @export

pty <- function(x, author = "International Potato Center",
                format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  classlist <- c("data.frame", "numeric", "aov", "lm")
  dirfiles <- system.file(package = "pepa")

  if (class(x) %in% classlist == FALSE) {
    fileRmd <- paste(dirfiles, "/rmd/na.Rmd", sep = "")  # Not available yet
    output <- "na"
  }
  if (class(x) == "data.frame") {
    fileRmd <- paste(dirfiles, "/rmd/df.Rmd", sep = "")  # for data.frames
    output <- "df"
  }
  if (class(x) == "numeric") {
    fileRmd <- paste(dirfiles, "/rmd/nt.Rmd", sep = "")  # for numeric traits
    output <- "nt"
  }
  if (sum(class(x) %in% c("aov", "lm")) > 0) {
    fileRmd <- paste(dirfiles, "/rmd/aov.Rmd", sep = "") # for linear models with aov or lm
    output <- "aov"
  }

  fileURL <- paste(dirfiles, "/rmd/", output, ".html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/", output, ".docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/", output, ".pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(x = x, author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
