#' Automatic report for a Line x Tester genetic design.
#'
#' Produces an automatic report for a Line x Tester genetic design analysis.
#' @param traits The traits to analize.
#' @param line The lines.
#' @param tester The testers.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model to data from a Line x Tester genetic design.
#' @return It returns an ANOVA and related quantities.
#' @examples
#' library(agricolae)
#' data(LxT)
#' repo.LxT("yield", "line", "tester", "replication", LxT)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.LxT <- function(traits, line, tester, rep, data,
                     title = "Automatic report for a Line x Tester genetic design",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/rmd/LxT.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/LxT.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/LxT.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/LxT.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  line = line,
                                  tester = tester,
                                  rep = rep,
                                  data = data,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
