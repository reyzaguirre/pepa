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

  format <- paste0(match.arg(format), "_document")
  classlist <- c("data.frame", "numeric", "aov", "lm")
  dirfiles <- system.file(package = "pepa")

  if (class(x) %in% classlist == FALSE) {
    fileRmd <- paste0(dirfiles, "/rmd/na.Rmd")  # Not available yet
    output <- "na"
  }
  if (class(x) == "data.frame") {
    fileRmd <- paste0(dirfiles, "/rmd/df.Rmd")  # for data.frames
    output <- "df"
  }
  if (class(x) == "numeric") {
    fileRmd <- paste0(dirfiles, "/rmd/nt.Rmd")  # for numeric traits
    output <- "nt"
  }
  if (sum(class(x) %in% c("aov", "lm")) > 0) {
    fileRmd <- paste0(dirfiles, "/rmd/aov.Rmd") # for linear models with aov or lm
    output <- "aov"
  }

  fileURL <- paste0(dirfiles, "/rmd/", output, ".html")
  fileDOCX <- paste0(dirfiles, "/rmd/", output, ".docx")
  filePDF <- paste0(dirfiles, "/rmd/", output, ".pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(x = x, author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
